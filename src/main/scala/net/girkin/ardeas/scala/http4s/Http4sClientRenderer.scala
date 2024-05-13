package net.girkin.ardeas.scala.http4s

import cats.*
import cats.implicits.given
import net.girkin.ardeas.Model.{Api, Default, HttpOperation, Parameter, PathSegment, Response, ResponseBody, Schema}
import net.girkin.ardeas.RenderUtils.*
import net.girkin.ardeas.scala.ScalaSpecifics
import ScalaSpecifics.MethodNaming.*
import Http4sSpecifics.*
import net.girkin.ardeas.scala.ScalaSpecifics.TypeNaming
import net.girkin.ardeas.scala.ClientRenderer

object Http4sClientRenderer extends ClientRenderer {
  private val headerParameter = ParameterDefinitionWithDefault("headers", "Headers", "Headers.empty")

  def renderClient(api: Api, packageName: Option[String], additionalImportPackages: Iterable[String]): String = {
    val packageAndImportsClause = packageAndImportsHeader(
      packageName,
      Vector(
        "cats.effect.Concurrent",
        "cats.Applicative",
        "org.http4s._",
        "org.http4s.Method._",
        "org.http4s.client.dsl.Http4sClientDsl",
        "Components.Schemas._"
      ).appendedAll(
        additionalImportPackages
      )
    ).getOrElse("")

    s"""$packageAndImportsClause
       |
       |${renderClientResponsesObject(api)}
       |
       |${renderClientInterface(api)}
       |
       |${renderClientClass(api)}
       |""".stripMargin
  }

  def renderClientInterface(api: Api): String = {
    val methodDefinitions = api.paths.map { httpOperation =>
      methodDefinitionForOperation(httpOperation, PathVarTypes, appendedParameters = List(headerParameter), effect = Some(str => s"F[$str]"))
    }

    s"""trait Client[F[_]] {
       |${indent(2)(methodDefinitions:_*)}
       |}""".stripMargin
  }

  def renderClientClass(api: Api) = {
    val renderedOperations = for {
      operation <- api.paths
    } yield {
      s"""${methodDefinitionForOperation(operation, PathVarTypes, appendedParameters = List(headerParameter), effect = Some(str => s"F[$str]"))} = {
         |${indent(2)(renderRequestForOperation(operation))}
         |}
         |""".stripMargin
    }

    s"""class ClientImpl[F[_]: Concurrent](
       |  client: org.http4s.client.Client[F],
       |  baseUri: Uri,
       |  defaultHeaders: Headers = Headers.empty
       |) extends Client[F] {
       |  import cats.syntax.functor._
       |  import org.http4s.Status._
       |  import Responses._
       |
       |  private val clientDsl = new Http4sClientDsl[F] {}
       |  import clientDsl._
       |
       |${indent(2, separator = doubleLineSeparator)(renderedOperations: _*)}
       |}""".stripMargin
  }

  def renderRequestForOperation(operation: HttpOperation): String = {
    val pathSegments = operation.path.segments.map {
      case PathSegment.StringSegment(str) => s"\"$str\""
      case PathSegment.TemplatedParameter(name) => s"$name.toString"
    }

    val renderedPath = pathSegments.mkString(" / ")
    val renderedUriWithoutQueryParams = if(renderedPath.isEmpty) {
      "baseUri"
    } else {
      s"baseUri / $renderedPath"
    }

    val queryParamAdders = operation.parameters.collect {
      case Parameter.QueryParameter(name, arrSchema @ Schema.Array(innerSchema), required) =>
        name -> "withQueryParam"
      case Parameter.QueryParameter(name, schema, required) => {
        if(required) {
          name -> "withQueryParam"
        } else {
          name -> "withOptionQueryParam"
        }
      }
    }.map { case (name, addParameterMethod) =>
      s".$addParameterMethod(\"$name\", $name)"
    }

    val url =
      s"val url = ($renderedUriWithoutQueryParams)${indent(2, prefix = lineSeparator)(queryParamAdders:_*)}"

    val method = operation.verb.toString.toUpperCase

    val requestBodyTerm = operation.requestBody.map { _ => s".withEntity(body)" }
    val requestHeadersTerm = ".withHeaders(defaultHeaders ++ headers)"
    val additionalTerms = requestBodyTerm.toVector
      .appended(requestHeadersTerm)

    val requestBuildLines =
      s"""val request = ${method}(url)
         |${indent(2)(additionalTerms:_*)}""".stripMargin

    val adtTopName = responseAdtTopName(operation)

    val specificResponseMatchTerms = operation.responses.collect { response =>
      response.httpCode match {
        case code: Int => s"case ${httpResponseCodeMatchTerm(code)} => ${renderResponseBuilder(adtTopName, response)}"
      }
    }

    val defaultResponseMatchTerm = {
      val responseBuilder = operation.responses
        .find(_.httpCode == Default)
        .fold(
          renderOtherResponseBuilder(adtTopName)
        ) ( response =>
          renderResponseBuilder(adtTopName, response)
        )
      s"case _ => ${responseBuilder}"
    }

    val responseMatchTerms = specificResponseMatchTerms.appended(defaultResponseMatchTerm).mkString(lineSeparator)

    s"""$url
       |
       |$requestBuildLines
       |
       |client.run(request).use { response =>
       |  response.status match {
       |${indent(4)(responseMatchTerms)}
       |  }
       |}
       |""".stripMargin
  }

  private def httpResponseCodeMatchTerm(code: Int): String = {
    Http4sResponseConstructorByHttpCode.getOrElse(code, s"Status($code)")
  }

  private def renderResponseBuilder(adtTopName: String, response: Response): String = {
    val responseCaseVariant = responseCaseClassName(adtTopName, Some(response.httpCode))
    val statusCodeConstructorArgument =
      Option.when(response.httpCode == Default)(
        "response.status.code"
      )

    val status405ConstructorArgument =
      Option.when(response.httpCode == 405)(
        "response.headers.get[org.http4s.headers.Allow].map(_.methods).getOrElse(Set.empty)"
      )

    val rawResponseConstructorArgument = Some("ResponseData(response)")

    val contentConstructorArgument =
      response.body match {
        case ResponseBody.Definition(jsonContent) => jsonContent.map(_ => "content")
        case ResponseBody.NamedRef(_) => Some("content")
      }

    val constructorArguments = Vector(statusCodeConstructorArgument, status405ConstructorArgument, contentConstructorArgument, rawResponseConstructorArgument).flatten.mkString(", ")

    // Responses described in schema
    val responseBuilderOpt: Option[String] = for {
      schemaScalaType <- response.body match {
        case ResponseBody.Definition(jsonContent) => jsonContent.map(s => TypeNaming.typeNameForNonAnonymousObjectSchema(s))
        case r @ ResponseBody.NamedRef(_) => Some(TypeNaming.typeNameFromReference(r, useFullyQualifiedRef = true))
      }
    } yield {
      s"""for {
         |  content <- response.as[$schemaScalaType]
         |} yield {
         |  ${responseCaseVariant}($constructorArguments)
         |}""".stripMargin
    }

    val noJsonContentResponseBuilder =
      s"""Applicative[F].pure(
         |  ${responseCaseVariant}($constructorArguments)
         |)""".stripMargin

    responseBuilderOpt.getOrElse(noJsonContentResponseBuilder)
  }

  private def renderOtherResponseBuilder(adtTopName: String): String = {
    val responseCaseVariant = responseCaseClassName(adtTopName, None)
    s"""Applicative[F].pure(
       |  ${responseCaseVariant}(response.status.code, response.body, ResponseData(response))
       |)""".stripMargin
  }
}
