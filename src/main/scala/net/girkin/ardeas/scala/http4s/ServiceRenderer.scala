package net.girkin.ardeas.scala.http4s

import net.girkin.ardeas.Model
import net.girkin.ardeas.Model.*
import net.girkin.ardeas.RenderUtils.*
import net.girkin.ardeas.scala.ScalaSpecifics
import net.girkin.ardeas.scala.ScalaSpecifics.MethodNaming.*
import net.girkin.ardeas.scala.ScalaSpecifics.TypeNaming
import net.girkin.ardeas.scala.ScalaSpecifics.TypeNaming.*
import net.girkin.ardeas.scala.http4s.Http4sSpecifics
import net.girkin.ardeas.scala.http4s.Http4sSpecifics.*

object ServiceRenderer {
  val ImplementationTraitName = "ServiceImpl"
  val ResponseConversionMethodName = "asHttp4sResponse"
  def renderService(api: Api, `package`: Option[String], additionalImportPackages: Iterable[String]) = {

    val packageAndImportsClause = packageAndImportsHeader(
      `package`,
      Vector(
        "cats.effect.Concurrent",
        "cats.Applicative",
        "cats.implicits._",
        "org.http4s.{HttpRoutes, Headers, Request, Response}",
        "org.http4s.dsl.Http4sDsl",
        "Components.Schemas._",
      ).appendedAll(
        additionalImportPackages
      )
    ).getOrElse("")

    s"""$packageAndImportsClause
        |
        |import scala.annotation.nowarn
        |
        |${Http4sSpecifics.renderServerResponsesObject(api)}
        |
        |${renderResponseConverters(api)}
        |
        |${renderImplementationTrait(api)}
        |
        |${renderQueryParameterMatchers(api)}
        |
        |${renderRouting(api)}
        |""".stripMargin
  }

  private def queryParameterMatcherName(operationName: String, parameterName: String) = {
    s"${capitalizeFirst(operationName)}${capitalizeFirst(parameterName)}QueryParamMatcher"
  }

  private def renderQueryParameterMatchers(api: Api) = {
    val parameters = for {
      operation <- api.paths
      operationName = methodNameForOperation(operation)
      parameter <- operation.parameters
    } yield {
      operationName -> parameter
    }

    val parameterMatchers = parameters.collect { case (operationName, Parameter.QueryParameter(parameterName, schema, required)) =>
      val parameterType = schema match {
        case Schema.Array(innerSchema) => ScalaSpecifics.TypeNaming.typeNameForNonAnonymousObjectSchema(innerSchema)
        case _ => ScalaSpecifics.TypeNaming.typeNameForNonAnonymousObjectSchema(schema)
      }
      val parameterMatcherType = (required, schema) match {
        case (_, schema: Schema.Array) => "ExplodingQueryParamDecoderMatcher"
        case (true, _) => "QueryParamDecoderMatcher"
        case (false, _) => "OptionalQueryParamDecoderMatcher"
      }

      s"object ${queryParameterMatcherName(operationName, parameterName)} extends $parameterMatcherType[$parameterType](\"${parameterName}\")"
    }

    s"""@nowarn object ParameterMatchers {
       |  import org.http4s.dsl.impl.{QueryParamDecoderMatcher, OptionalQueryParamDecoderMatcher}
       |  import org.http4s.{QueryParameterValue, QueryParamDecoder}
       |
       |  abstract class ExplodingQueryParamDecoderMatcher[T: QueryParamDecoder](name: String) {
       |    def unapply(
       |      params: Map[String, collection.Seq[String]]
       |    ): Option[Vector[T]] = {
       |      params.get(name) match {
       |        case Some(values) =>
       |          values.toVector
       |            .traverse(s => QueryParamDecoder[T].decode(QueryParameterValue(s)))
       |            .toOption
       |        case None =>
       |          Some(Vector.empty)
       |      }
       |    }
       |  }
       |
       |${indent(2)(parameterMatchers:_*)}
       |}""".stripMargin
  }

  private[http4s] def renderRouting(api: Api) = {
    val matchers = api.paths.map { singlePath =>
      renderSingleOperation(singlePath)
    }

    s"""object Service {
       |  def makeRoutes[F[_]: Concurrent, I](
       |    impl: $ImplementationTraitName[F, I],
       |    preProcessHeaders: Headers => F[Either[Response[F], I]],
       |    postProcessHeaders: Headers => Headers = identity
       |  ) = {
       |    def withPreProcessedHeaders(req: Request[F])(action: RequestData[F, I] => F[Response[F]]) = {
       |      for {
       |        preProcessResult <- preProcessHeaders(req.headers)
       |        result <- preProcessResult.fold(
       |          response => Concurrent[F].pure(response),
       |          data => action(RequestData(data, req))
       |        )
       |      } yield {
       |        result
       |      }
       |    }
       |
       |    val dsl = new Http4sDsl[F]{}
       |    val responseConverters = new ResponseConverters[F](dsl)
       |    import dsl._
       |    import responseConverters._
       |    import ParameterMatchers._
       |
       |    HttpRoutes.of[F] {
       |${indent(6)(matchers: _*)}
       |    }
       |  }
       |}""".stripMargin
  }

  private[http4s] def renderImplementationTrait(api: Api) = {
    val methods = api.paths.map { operation =>
      methodDefinitionForOperation(
        operation,
        PathVarTypes,
        prependedParameters = Map("request" -> "RequestData[F, I]"),
        appendedParameters = List(ParameterDefinitionWithDefault("headers", "Headers", "Headers.empty")),
        effect = Some("F")
      )
    }

    s"""case class RequestData[F[_], I](
       |  headerData: I,
       |  raw: Request[F]
       |)
       |
       |trait $ImplementationTraitName[F[_], I] {
       |  import Responses._
       |
       |${indent(2)(methods:_*)}
       |}""".stripMargin
  }

  private def renderResponseConverterCase(adtTopName: String)(response: Response) = {
    val contentReference = response.body match {
      case ResponseBody.Definition(jsonContent) => jsonContent.map(_ => "r.content")
      case ResponseBody.NamedRef(_) => Some("r.content")
    }
    val allowMehodsReference = "r.allowMethods"
    val withEntityClause = contentReference.map(cr => s".withEntity($cr)").getOrElse("")
    val defaultResponseConstructor = s"implicitly[Applicative[F]].pure(Response(org.http4s.Status(r.httpCode))$withEntityClause)"

    val specificResponseConstructor = response.httpCode match {
      case code : Int if code == 405 =>
        Http4sResponseConstructorByHttpCode.get(code).map(constructor =>
          val subsequentContentReference = contentReference.map(_.prependedAll(", ")).getOrElse("")
          s"$constructor(org.http4s.headers.Allow($allowMehodsReference)${subsequentContentReference})"
        )
      case code: Int =>
        Http4sResponseConstructorByHttpCode.get(code).map(constructor =>
          s"$constructor(${contentReference.getOrElse("")})"
        )
      case _ => None
    }

    val responseConstructor = specificResponseConstructor.getOrElse(defaultResponseConstructor)

    s"case r: ${Http4sSpecifics.responseCaseClassName(adtTopName, Some(response.httpCode))} => $responseConstructor"
  }

  def renderResponseConverters(api: Api) = {
    val responseConverters = for {
      operation <- api.paths
    } yield {
      val adtTopName = responseAdtTopName(operation)
      val converterCases = operation.responses.map(renderResponseConverterCase(adtTopName))

      s"""def $ResponseConversionMethodName(response: $adtTopName): F[Response[F]] = {
         |  response match {
         |${indent(4)(converterCases: _*)}
         |  }
         |}""".stripMargin
    }

    s"""@nowarn class ResponseConverters[F[_]: Concurrent](dsl: Http4sDsl[F]) {
       |  import Responses._
       |  import dsl._
       |  import org.http4s.Response
       |
       |${indent(2)(responseConverters: _*)}
       |}""".stripMargin

  }

  private[http4s] def renderSingleOperation(operation: HttpOperation): String = {
    val methodName = methodNameForOperation(operation)
    val method = operation.verb.toString.toUpperCase()
    val pathParameterExtractorsMap: Map[String, String] =
      operation.parameters.collect {
        case Parameter.PathParameter(name, schema) =>
          name -> PathVarExtractors(schema)
      }.collect { case (name, Some(extractor)) => name -> extractor }
        .toMap

    val path = operation.path.segments
      .map {
        case PathSegment.StringSegment(str) => s"\"$str\""
        case PathSegment.TemplatedParameter(name) => {
          pathParameterExtractorsMap.get(name).map { extractor =>
            s"$extractor($name)"
          }.getOrElse(
            s"$name"
          )
        }
      }.mkString(" / ")
    val pathParameters = operation.path.segments.collect { case PathSegment.TemplatedParameter(name) => name }
    val queryParameterMatchers = operation.parameters.collect {
      case Parameter.QueryParameter(parameterName, schema, required) =>
        s"${queryParameterMatcherName(methodName, parameterName)}($parameterName)"
    }
    val queryParameters = operation.parameters.collect {
      case Parameter.QueryParameter(name, schema, required) => name
    }
    val queryParameterMatcherClause = queryParameterMatchers.mkStringIfNonEmpty(" :? ", " +& ", "")

    def implementationCallClause(bodyPresent: Boolean) = {
      val parameterList = List.concat(
        List("requestData"),
        pathParameters,
        (if(bodyPresent) List("body") else List.empty),
        queryParameters
      )
      s"impl.$methodName(${parameterList.mkString(", ")})"
    }

    operation.requestBody.fold (
      s"""case req @ ${method} -> Root / ${path}${queryParameterMatcherClause} => withPreProcessedHeaders(req) { requestData =>
         |  ${implementationCallClause(false)}
         |    .flatMap($ResponseConversionMethodName)
         |    .map(response => response.withHeaders(postProcessHeaders(response.headers)))
         |}""".stripMargin
    ) { requestBody =>
      val typeNameForRequestBody = requestBody match {
        case ref @ Model.RequestBody.NamedRef(_) => typeNameFromReference(ref, useFullyQualifiedRef = true)
        case Model.RequestBody.Definition(required, jsonContent) =>
          val typeName = jsonContent.map(ScalaSpecifics.TypeNaming.typeNameForRequestBody).getOrElse("String")
          TypeNaming.typeForOptional(typeName, required)
      }
      s"""case req @ ${method} -> Root / ${path}${queryParameterMatcherClause} => withPreProcessedHeaders(req) { requestData =>
         |  for {
         |    body <- req.as[$typeNameForRequestBody]
         |    resp <- ${implementationCallClause(true)}
         |    http4sResponse <- $ResponseConversionMethodName(resp)
         |  } yield {
         |    http4sResponse.withHeaders(postProcessHeaders(http4sResponse.headers))
         |  }
         |}""".stripMargin
    }
  }

}
