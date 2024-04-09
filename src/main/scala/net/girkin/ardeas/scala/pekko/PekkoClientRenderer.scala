package net.girkin.ardeas.scala.pekko

import net.girkin.ardeas.Model
import net.girkin.ardeas.Model.*
import net.girkin.ardeas.Model.Schema.*
import net.girkin.ardeas.RenderUtils.{doubleLineSeparator, indent}
import net.girkin.ardeas.scala.ScalaSpecifics.MethodNaming.responseAdtTopName
import net.girkin.ardeas.scala.ScalaSpecifics.Rendering.CaseClassFieldDescription
import net.girkin.ardeas.scala.ClientRenderer
import net.girkin.ardeas.RenderUtils.*
import net.girkin.ardeas.scala.pekko.PekkoSpecifics.*
import net.girkin.ardeas.scala.ScalaSpecifics.*

object PekkoClientRenderer extends ClientRenderer {
  def renderClient(api: Api, packageName: Option[String], additionalImportPackages: Iterable[String]): String = {
    val packageAndImportsClause = packageAndImportsHeader(
      packageName,
      Vector(
        "org.apache.pekko.actor.ActorSystem",
        "org.apache.pekko.http.scaladsl.Http",
        "org.apache.pekko.http.scaladsl.marshalling.Marshal",
        "org.apache.pekko.http.scaladsl.model.{HttpEntity, RequestEntity, HttpHeader, HttpMethods, HttpRequest, HttpResponse, ResponseEntity, StatusCodes, Uri}",
        "org.apache.pekko.http.scaladsl.model.Uri.Query",
        "org.apache.pekko.http.scaladsl.unmarshalling.Unmarshal",
        "Components.Schemas._",
        "scala.concurrent.ExecutionContext",
        "scala.concurrent.Future",
      ).appendedAll(
        additionalImportPackages
      )
    ).getOrElse("")

    val responseObjects = renderClientResponsesObject(api)
    val clientInterface = renderClientInterface(api)
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

  def renderClientResponsesObject(api: Api) = {
    val responses = for {
      operation <- api.paths
    } yield {
      val adtTopName = responseAdtTopName(operation)
      val standardFields = Vector(CaseClassFieldDescription("responseData", "ResponseData"))

      val unexpectedResponseCaseClass: Option[String] = Option.when(
        !operation.responses.exists(_.httpCode == Default)
      )(
        renderOtherResponseCaseClass(adtTopName, standardFields)
      )

      val caseClasses = operation.responses
        .map(renderResponseCaseClass(adtTopName, standardFields))
        .appendedAll(unexpectedResponseCaseClass)

      s"""sealed trait $adtTopName extends Product with Serializable
         |${indent(0)(caseClasses: _*)}
         |
         |""".stripMargin + lineSeparator
    }

    s"""object Responses {
       |  case class ResponseData(
       |    raw: HttpResponse
       |  ) {
       |    def headers: Seq[HttpHeader] = raw.headers
       |  }
       |
       |${indent(2, separator = doubleLineSeparator)(responses: _*)}
       |}""".stripMargin
  }

  private def renderResponseCaseClass(adtTopName: String, standardFields: Vector[CaseClassFieldDescription])(response: Response): String = {
    val caseClassName = responseCaseClassName(adtTopName, Some(response.httpCode))

    // HTTP 405 requires special treatment, response must include Allow header
    // http4s MethodNotAllowed constructor requires this header

    val additionalResponseFields: Vector[CaseClassFieldDescription] = response.httpCode match {
      case Default => Vector(
        CaseClassFieldDescription("httpCode", "Int")
      )
      case 405 => Vector(
        CaseClassFieldDescription("allowMethods", "Set[org.http4s.Method]")
      )
      case _ => Vector.empty
    }

    val responseFields = Vector.concat(
      additionalResponseFields,
      TypeNaming.typeNameForResponseBody(response.body).map { typeDefinition =>
        CaseClassFieldDescription("content", typeDefinition)
      },
      standardFields
    )

    Rendering.renderCaseClass(
      caseClassName,
      responseFields,
      classAccessModifier = Some("final"),
      extendsClasses = Seq(adtTopName)
    )
  }

  private def renderOtherResponseCaseClass(adtTopName: String, standardFields: Vector[CaseClassFieldDescription]): String = {
    val otherResponseFields = Vector(
      CaseClassFieldDescription("httpCode", "Int"),
      CaseClassFieldDescription("content", "ResponseEntity"),
    )
    Rendering.renderCaseClass(
      responseCaseClassName(adtTopName, None),
      otherResponseFields ++ standardFields,
      classAccessModifier = Some("final"),
      extendsClasses = Seq(adtTopName)
    )
  }

  private def responseCaseClassName(adtTopName: String, httpCode: Option[Int | Default.type]) = {
    val responseName = httpCode.map(_.toString).getOrElse("Other")
    s"$adtTopName${responseName}"
  }

  private def renderClientInterface(api: Api) = {
    val methodDefinitions = api.paths.map { httpOperation =>
      methodDefinitionForOperation(httpOperation, PathVarTypes)
    }

    s"""trait Client {
       |${indent(2)(methodDefinitions: _*)}
       |}""".stripMargin
  }

  private def methodDefinitionForOperation(
    operation: HttpOperation,
    pathVarTypesTranslator: StandardType => String,
    additionalParameters: Map[String, String] = Map.empty
  ): String = {
    val methodName = MethodNaming.methodNameForOperation(operation)
    val pathParametersTypes: Map[String, String] =
      operation.parameters.collect {
        case Parameter.PathParameter(name, schema) =>
          name -> pathVarTypesTranslator(schema)
      }.map {
        case (parameterName, scalaType) => parameterName -> scalaType
      }.toMap

    val pathParameterDefinitions: Seq[String] = for {
      parameterName <- operation.path.segments.collect { case PathSegment.TemplatedParameter(name) => name }
    } yield {
      val parameterType = pathParametersTypes.getOrElse(parameterName, "String")
      s"$parameterName: $parameterType"
    }

    val bodyParameter = operation.requestBody.map {
      case ref@Model.RequestBody.NamedRef(_) =>
        s"body: ${TypeNaming.typeNameFromReference(ref, useFullyQualifiedRef = true)}"

      case rb@Model.RequestBody.Definition(_, _) =>
        val bodyParameterTypeName = rb.jsonContent.fold(
          "String"
        ) { schema =>
          TypeNaming.typeNameForRequestBody(schema)
        }
        val bodyParameterType = TypeNaming.typeForOptional(bodyParameterTypeName, rb.required)
        s"body: $bodyParameterType"
    }

    val queryParameterDefinitions = operation.parameters.collect {
      case Parameter.QueryParameter(parameterName, arrSchema@Schema.Array(innerSchema), required) => {
        parameterName -> TypeNaming.typeDefinitionForArray(arrSchema)
      }
      case Parameter.QueryParameter(parameterName, schema, required) => {
        parameterName -> TypeNaming.typeForOptional(TypeNaming.typeNameForNonAnonymousObjectSchema(schema), required)
      }
    }.map { case (parameterName, typeDefinition) =>
      s"$parameterName: $typeDefinition"
    }

    val headerParameter = s"headers: Seq[HttpHeader] = Seq.empty"
    val additionalParameterDefinitions = additionalParameters.map { case (name, typeName) => s"$name: $typeName" }
    val parameterDefinitions = Vector.concat(
      additionalParameterDefinitions,
      pathParameterDefinitions,
      bodyParameter,
      queryParameterDefinitions,
      List(headerParameter)
    )
    val returnType = responseAdtTopName(operation, fullyQualified = true)
    s"def $methodName(${parameterDefinitions.mkString(", ")}): Future[${returnType}]"
  }

  private def renderSingleOperation(operation: HttpOperation): String = {
    val methodDefinition = methodDefinitionForOperation(operation, PathVarTypes)
    val responseAdtTopName = MethodNaming.responseAdtTopName(operation, fullyQualified = true)

    val requestBuilder = requestBuilderLines(operation)

    val knownResponseMatchers = operation.responses.collect {
      case Response(statusCode: Int, responseBody) =>
        s"""case ${renderResponseMatchTerm(statusCode)} =>
           |${indent(2)(renderResponseBuilder(responseAdtTopName, statusCode, responseBody))}""".stripMargin
    }

    val otherResponses = operation.responses.collectFirst {
      case Response(statusCode@Default, responseBody) =>
        s"""case _ =>
           |${indent(2)(renderResponseBuilder(responseAdtTopName, statusCode, responseBody))}""".stripMargin
    }.getOrElse (
      s"""case _ =>
         |${indent(2)(renderOtherResponseBuilder(responseAdtTopName))}""".stripMargin
    )
    val responses = knownResponseMatchers.appended(otherResponses)

    val queryParamLines = operation.parameters.collect {
      case Parameter.QueryParameter(name, schema, true) =>
        s".updated(\"$name\", ${name}.toString)"
      case Parameter.QueryParameter(name, schema, false) =>
        s".concat(${name}.map(x => \"$name\" -> x.toString))"
    }
    val queryParameters = if (queryParamLines.nonEmpty) {
      Some(
        s"""val queryParameters = Map.empty[String, String]
           |${indent(2)(queryParamLines: _*)}""".stripMargin
      )
    } else {
      None
    }
    val pathSegments = operation.path.segments.map {
      case PathSegment.StringSegment(str) => s"\"$str\""
      case PathSegment.TemplatedParameter(name) => s"$name.toString"
    }
    val renderedPath = pathSegments.prepended("baseUri.path").mkString(" / ")
    val renderedUriWithoutQueryParams = if (renderedPath.isEmpty) {
      "val requestUrl = baseUri"
    } else {
      s"val requestUrl = baseUri.withPath($renderedPath)"
    }
    val renderedRequestUrlBuilderSegment = queryParameters.fold(renderedUriWithoutQueryParams) { queryParams =>
      s"""${queryParams}
         |${renderedUriWithoutQueryParams}
         |  .withQuery(Query(queryParameters))
         |""".stripMargin
    }

    s"""${methodDefinition} = {
       |${indent(2)(renderedRequestUrlBuilderSegment)}
       |  for {
       |${indent(4)(requestBuilder)}
       |    rawResponse <- Http().singleRequest(request)
       |    result <- rawResponse.status match {
       |${indent(6)(responses:_*)}
       |    }
       |  } yield {
       |    result
       |  }
       |}
       |""".stripMargin
  }

  def requestBuilderLines(operation: HttpOperation): String = {
    val method = s"HttpMethods.${operation.verb.toString.toUpperCase}"
    operation.requestBody.fold(
      s"""request <- Future.successful(
         |  HttpRequest(
         |    $method,
         |    requestUrl,
         |    headers = defaultHeaders.appended(headers),
         |    HttpEntity.Empty
         |  )
         |)""".stripMargin
    ) { entityBody =>
      s"""entity <- Marshal(body).to[RequestEntity]
         |request = HttpRequest(
         |  $method,
         |  requestUrl,
         |  headers = defaultHeaders.appended(headers),
         |  entity
         |)""".stripMargin
    }
  }

  private def renderResponseMatchTerm(statusCode: Int): String = {
    PekkoSpecifics.ResponseConstructorByHttpCode.get(statusCode)
      .map { term =>
        s"StatusCodes.${term}"
      }.getOrElse(
        s"StatusCodes.CustomStatusCode(${statusCode})"
      )
  }

  private def renderOtherResponseBuilder(responseAdtTopName: String) = {
    val responseObjectTypeName = responseCaseClassName(responseAdtTopName, None)
    s"""Future.successful(
       |  ${responseObjectTypeName}(ResponseData(rawResponse))
       |)""".stripMargin
  }

  private def renderResponseBuilder(responseAdtTopName: String, statusCode: HttpCode, responseBody: ResponseBody): String = {
    val responseEntityTypeNameOpt = responseBody match {
      case ResponseBody.Definition(jsonContent) => jsonContent.map(s => TypeNaming.typeNameForNonAnonymousObjectSchema(s))
      case r @ ResponseBody.NamedRef(_) => Some(TypeNaming.typeNameFromReference(r, useFullyQualifiedRef = true))
    }
    val responseObjectTypeName = responseCaseClassName(responseAdtTopName, Some(statusCode))

    responseEntityTypeNameOpt.fold (
      s"""Future.successful(
         |  ${responseObjectTypeName}(ResponseData(rawResponse))
         |)""".stripMargin
    ) { entityTypeName =>

      val constructorArguments = "entity, ResponseData(rawResponse)"

      s"""Unmarshal(rawResponse.entity).to[${entityTypeName}]
         |  .map { entity =>
         |    ${responseObjectTypeName}(${constructorArguments})
         |  }
         |""".stripMargin
    }
  }

  private def renderClientImplementation(api: Api): String = {
    val operationImplementations = api.paths.map {
      renderSingleOperation
    }

    s"""class ClientImpl(
       |  baseUri: Uri,
       |  defaultHeaders: Seq[HttpHeader] = Seq.empty
       |) ( implicit
       |  ec: ExecutionContext,
       |  as: ActorSystem
       |) extends Client {
       |
       |import Responses._
       |
       |${indent(2, separator = doubleLineSeparator)(operationImplementations: _*)}
       |}""".stripMargin
  }
}

object PekkoSpecifics {
  val PathVarTypes: StandardType => String = {
    case StandardType("integer", Some("int64")) => "Long"
    case StandardType("integer", _) => "Int"
    case _ => "String"
  }

  val ResponseConstructorByHttpCode = Map[Int, String](
    100 -> "Continue",
    101 -> "SwitchingProtocols",
    102 -> "Processing",
    103 -> "EarlyHints",

    200 -> "OK",
    201 -> "Created",
    202 -> "Accepted",
    203 -> "NonAuthoritativeInformation",
    204 -> "NoContent",
    205 -> "ResetContent",
    206 -> "PartialContent",
    207 -> "MultiStatus",
    208 -> "AlreadyReported",
    226 -> "IMUsed",

    300 -> "MultipleChoices",
    301 -> "MovedPermanently",
    302 -> "Found",
    303 -> "SeeOther",
    304 -> "NotModified",
    305 -> "UseProxy",
    307 -> "TemporaryRedirect",
    308 -> "PermanentRedirect",

    400 -> "BadRequest",
    401 -> "Unauthorized",
    402 -> "PaymentRequired",
    403 -> "Forbidden",
    404 -> "NotFound",
    405 -> "MethodNotAllowed",
    406 -> "NotAcceptable",
    407 -> "ProxyAuthenticationRequired",
    408 -> "RequestTimeout",
    409 -> "Conflict",
    410 -> "Gone",
    411 -> "LengthRequired",
    412 -> "PreconditionFailed",
    413 -> "PayloadTooLarge",
    414 -> "UriTooLong",
    415 -> "UnsupportedMediaType",
    416 -> "RangeNotSatisfiable",
    417 -> "ExpectationFailed",
    418 -> "ImATeapot",
    420 -> "EnhanceYourCalm",
    421 -> "MisdirectedRequest",
    422 -> "UnprocessableEntity",
    423 -> "Locked",
    424 -> "FailedDependency",
    425 -> "TooEarly",
    426 -> "UpgradeRequired",
    428 -> "PreconditionRequired",
    429 -> "TooManyRequests",
    431 -> "RequestHeaderFieldsTooLarge",
    449 -> "RetryWith",
    450 -> "BlockedByParentalControls",
    451 -> "UnavailableForLegalReasons",

    500 -> "InternalServerError",
    501 -> "NotImplemented",
    502 -> "BadGateway",
    503 -> "ServiceUnavailable",
    504 -> "GatewayTimeout",
    505 -> "HttpVersionNotSupported",
    506 -> "VariantAlsoNegotiates",
    507 -> "InsufficientStorage",
    508 -> "LoopDetected",
    509 -> "BandwidthLimitExceeded",
    510 -> "NotExtended",
    511 -> "NetworkAuthenticationRequired",
    598 -> "NetworkReadTimeout",
    599 -> "NetworkConnectTimeout",
  )
}