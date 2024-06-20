package net.girkin.ardeas.scala.sttp3

import net.girkin.ardeas.Model
import net.girkin.ardeas.Model.{Default, Parameter, PathSegment, Response, ResponseBody, Schema}
import net.girkin.ardeas.RenderUtils.*
import net.girkin.ardeas.scala.ClientRenderer
import net.girkin.ardeas.scala.ScalaSpecifics.*
import net.girkin.ardeas.scala.ScalaSpecifics.Rendering.CaseClassFieldDescription
import Sttp3Specifics.*
import net.girkin.ardeas.Model.Schema.StandardType
import net.girkin.ardeas.scala.ScalaSpecifics.MethodNaming.ParameterDefinitionWithDefault

object Sttp3ClientRenderer extends ClientRenderer {
  override def renderClient(api: Model.Api, packageName: Option[String], additionalImportPackages: Iterable[String]): String = {
    val packageAndImportsClause = packageAndImportsHeader(
      packageName,
      Vector(
        "sttp.client3._",
        "sttp.model._",
        "Components.Schemas._"
      ).appendedAll(
        additionalImportPackages
      )
    ).getOrElse("")

    val responseObjects = renderClientResponseObjects(api)

    val clientInterface = renderClientInterface(api, PathVarTypes, appendParameters = List(ParameterDefinitionWithDefault("headers", "Seq[Header]", "Seq.empty")))

    val clientImplementation = renderClientImplementation(api)

    s"""${packageAndImportsClause}
       |
       |${responseObjects}
       |
       |${clientInterface}
       |
       |${clientImplementation}
       |""".stripMargin
  }

  private def renderClientResponseObjects(api: Model.Api) = {
    val responses = for {
      operation <- api.paths
    } yield {
      val adtTopName = MethodNaming.responseAdtTopName(operation)
      val unexpectedResponseCaseClass: Option[String] = Option.when(
        !operation.responses.exists(_.httpCode == Default)
      )(
        renderOtherResponseObject(adtTopName)
      )
      val caseClasses = operation.responses
        .map(renderResponseCaseClass(adtTopName))
        .appendedAll(unexpectedResponseCaseClass)
        .appended(renderDeserializationErrorResponseObject(adtTopName))

      s"""sealed trait $adtTopName extends Product with Serializable {
         |  def headers: Seq[Header]
         |  def statusCode: Int
         |}
         |${indent(0)(caseClasses: _*)}
         |
         |""".stripMargin + lineSeparator
    }

    s"""object Responses {
       |${indent(2, separator = doubleLineSeparator)(responses: _*)}
       |}""".stripMargin
  }

  private def renderOtherResponseObject(adtTopName: String): String = {
    s"""final case class ${responseCaseClassName(adtTopName, None)}(
       |  content: String,
       |  statusCode: Int,
       |  headers: Seq[Header]
       |) extends ${adtTopName}""".stripMargin
  }

  private def deserializationErrorResponseClassName(adtTopName: String) = {
    s"${adtTopName}DeserializationError"
  }

  private def renderDeserializationErrorResponseObject(adtTopName: String): String = {
    s"""final case class ${deserializationErrorResponseClassName(adtTopName)}[Err](
       |  content: String,
       |  statusCode: Int,
       |  headers: Seq[Header],
       |  error: Err
       |) extends ${adtTopName}""".stripMargin
  }

  private def responseCaseClassName(adtTopName: String, httpCode: Option[Int | Default.type]) = {
    val responseName = httpCode.map(_.toString).getOrElse("Other")
    s"$adtTopName${responseName}"
  }

  private def renderResponseCaseClass(adtTopName: String)(response: Response): String = {
    val caseClassName = responseCaseClassName(adtTopName, Some(response.httpCode))

    val responseFields = Vector.concat(
      TypeNaming.typeNameForResponseBody(response.body).map { typeDefinition =>
        CaseClassFieldDescription("content", typeDefinition)
      },
      List(
        CaseClassFieldDescription("headers", "Seq[Header]")
      )
    )

    val caseClassBody = response.httpCode match {
      case Default => None
      case statusCode => Some(s"val statusCode = ${statusCode}")
    }

    Rendering.renderCaseClass(
      caseClassName,
      responseFields,
      classAccessModifier = Some("final"),
      extendsClasses = Seq(adtTopName),
      methodsBody = caseClassBody
    )
  }

  private def renderClientInterface(
    api: Model.Api,
    pathVarTypesTranslator: StandardType => String,
    appendParameters: List[ParameterDefinitionWithDefault] = List.empty
  ): String = {
    val methodDefinitions = api.paths.map { httpOperation =>
      MethodNaming.methodDefinitionForOperation(httpOperation, pathVarTypesTranslator, appendedParameters = appendParameters, effect = Some(str => s"F[$str]"))
    }

    s"""trait Client[F[_]] {
       | import Responses._
       |
       |${indent(2)(methodDefinitions: _*)}
       |}""".stripMargin
  }


  private def renderClientImplementation(api: Model.Api): String = {
    val methodsImplementation = api.paths.map { operation =>
      renderSingleOperation(operation)
    }

    s"""class ClientImpl[F[_], P](
       |  baseUri: Uri,
       |  backend: SttpBackend[F, P],
       |  defaultHeaders: Seq[Header] = Seq.empty
       |) extends Client[F] {
       |  import Responses._
       |
       |${indent(2, separator = doubleLineSeparator)(methodsImplementation: _*)}
       |}""".stripMargin
  }

  private def renderSingleOperation(operation: Model.HttpOperation) = {
    val methodDefinition = MethodNaming.methodDefinitionForOperation(
      operation,
      PathVarTypes,
      appendedParameters = List(ParameterDefinitionWithDefault("headers", "Seq[Header]", "Seq.empty")),
      effect = Some(str => s"F[$str]")
    )
    val responseAdtTopName = MethodNaming.responseAdtTopName(operation, fullyQualified = true)

    val pathSegments = operation.path.segments.map {
      case PathSegment.StringSegment(str) => s"$str"
      case PathSegment.TemplatedParameter(name) => "$" + name
    }
    val path = pathSegments.mkString("/")
    val queryParameterAdders = operation.parameters.collect {
      case Parameter.QueryParameter(name, arrSchema @ Schema.Array(innerSchema), _) =>
        s".appendedAll(${name}.map(item => \"$name\" -> item.toString))"
      case Parameter.QueryParameter(name, schema, true) =>
        s".appended(\"$name\" -> ${name}.toString)"
      case Parameter.QueryParameter(name, schema, false) =>
        s".appendedAll(${name}.map(item => \"$name\" -> item.toString))"
    }


    val queryParametersInitializerOpt = Option.when(queryParameterAdders.nonEmpty) {
      s"""val queryParameters = List.empty[(String, String)]
         |${indent(2)(queryParameterAdders: _*)}""".stripMargin
    }
    val queryParametersAdder = ".withParams(QueryParams.fromSeq(queryParameters))"

    val url = s"val url = uri\"$$baseUri/$path\""

    val urlBuilder  = queryParametersInitializerOpt.fold(
      url
    ) { queryParametersInitializer =>
      s"""$queryParametersInitializer
         |$url
         |${indent(2)(queryParametersAdder)}""".stripMargin
    }

    val method = operation.verb.toString.toLowerCase

    val otherResponseClassName = operation.responses.collectFirst {
      case r if r.httpCode == Model.Default => responseCaseClassName(responseAdtTopName, Some(Model.Default))
    }.getOrElse(
      responseCaseClassName(responseAdtTopName, None)
    )

    val otherResponseProcessor = operation.responses.collectFirst {
      case Response(Model.Default, body) => renderDefaultResponseProcessor(responseAdtTopName, body)
    }.getOrElse(
      renderOtherResponseProcessor(responseAdtTopName)
    )

    val responseProcessors = operation.responses.collect {
      case Response(httpCode: Int, body) => renderResponseProcessor(responseAdtTopName, httpCode, body)
    }

    val allResponseProcessors = responseProcessors.appended(otherResponseProcessor)
    val bodySender = operation.requestBody.map { _ =>
      ".body(serialize(body))"
    }

    val methodAndBodySegment =
      s""".$method(url)""" + bodySender.map(str => lineSeparator + str).getOrElse("")

    s"""${methodDefinition} = {
       |${indent(2)(urlBuilder)}
       |  val responseF = basicRequest
       |${indent(4)(methodAndBodySegment)}
       |    .headers(defaultHeaders.concat(headers):_*)
       |    .response(asStringAlways)
       |    .send(backend)
       |
       |  backend.responseMonad.map(responseF) { response =>
       |    response.code.code match {
       |${indent(6)(allResponseProcessors:_*)}
       |    }
       |  }
       |}""".stripMargin
  }

  private def renderDefaultResponseProcessor(responseAdtTopName: String, responseBody: ResponseBody) = {
    val caseClassName = responseCaseClassName(responseAdtTopName, Some(Model.Default))
    val entityConstructorArgumentOpt = renderResponseEntityConstructorExpression(responseBody)
    val deserializationErrorCaseClassName = deserializationErrorResponseClassName(responseAdtTopName)
    entityConstructorArgumentOpt.fold (
      s"""case _ =>
         |  ${caseClassName}(response.headers)
         |""".stripMargin
    ) ( deserializer =>
      s"""case _ =>
         |  $deserializer.fold(
         |    error => $deserializationErrorCaseClassName(response.body, response.code.code, response.headers, error),
         |    entity => ${caseClassName}(entity, response.headers)
         |  )
         |""".stripMargin
    )
  }

  private def renderOtherResponseProcessor(responseAdtTopName: String) = {
    val caseClassName = responseCaseClassName(responseAdtTopName, None)
    s"""case _ =>
       |  ${caseClassName}(response.body, response.code.code, response.headers)
       |""".stripMargin
  }

  private def renderResponseProcessor(responseAdtTopName: String, statusCode: Int, responseBody: ResponseBody) = {
    val entityConstructorArgumentOpt = renderResponseEntityConstructorExpression(responseBody)

    val caseClassName = responseCaseClassName(responseAdtTopName, Some(statusCode))
    val deserializationErrorCaseClassName = deserializationErrorResponseClassName(responseAdtTopName)
    entityConstructorArgumentOpt.fold(
      s"""case ${statusCode} =>
         |  ${caseClassName}(response.headers)
         |""".stripMargin
    ) ( deserializer =>
      s"""case ${statusCode} =>
         |  $deserializer.fold(
         |    error => $deserializationErrorCaseClassName(response.body, response.code.code, response.headers, error),
         |    entity => ${caseClassName}(entity, response.headers)
         |  )
         |""".stripMargin
    )
  }

  private def renderResponseEntityConstructorExpression(responseBody: ResponseBody): Option[String] = {
    val responseEntityTypeNameOpt = responseBody match {
      case ResponseBody.Definition(jsonContent) => jsonContent.map(s => TypeNaming.typeNameForNonAnonymousObjectSchema(s))
      case r@ResponseBody.NamedRef(_) => Some(TypeNaming.typeNameFromReference(r, useFullyQualifiedRef = true))
    }
    responseEntityTypeNameOpt.map(typeName =>
      s"deserialize[$typeName](response.body)"
    )
  }

}
