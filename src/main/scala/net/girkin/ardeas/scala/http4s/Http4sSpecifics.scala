package net.girkin.ardeas.scala.http4s

import net.girkin.ardeas.Model.*
import net.girkin.ardeas.Model.Schema.*
import net.girkin.ardeas.RenderUtils.*
import net.girkin.ardeas.scala.ScalaSpecifics
import net.girkin.ardeas.scala.ScalaSpecifics.MethodNaming.responseAdtTopName
import net.girkin.ardeas.scala.ScalaSpecifics.Rendering.CaseClassFieldDescription
import net.girkin.ardeas.scala.ScalaSpecifics.TypeNaming.{typeNameForNonAnonymousObjectSchema, typeNameForResponseBody}

object Http4sSpecifics {
  val PathVarExtractors: StandardType => Option[String] = {
    case StandardType("integer", Some("int64")) => Some("LongVar")
    case StandardType("integer", _) => Some("IntVar")
    case _ => None
  }

  val PathVarTypes: StandardType => String = {
    case StandardType("integer", Some("int64")) => "Long"
    case StandardType("integer", _) => "Int"
    case _ => "String"
  }

  val Http4sResponseConstructorByHttpCode = Map[Int, String](
    100 -> "Continue",
    101 -> "SwitchingProtocols",
    102 -> "Processing",
    103 -> "EarlyHints",

    200 -> "Ok",
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

  def responseCaseClassName(adtTopName: String, httpCode: Option[Int | Default.type], suffix: Option[String] = None) = {
    val responseName = httpCode.map(_.toString).getOrElse("Other")
    val actualSuffix = suffix.getOrElse("")
    s"$adtTopName${responseName}${actualSuffix}"
  }

  def renderServerResponsesObject(api: Api) = {
    val responses = for {
      operation <- api.paths
    } yield {
      val adtTopName = responseAdtTopName(operation)
      val standardFields = Vector(CaseClassFieldDescription("headers", "Headers"))

      val caseClasses = operation.responses
        .map(renderResponseCaseClass(adtTopName, standardFields))

      s"""sealed trait $adtTopName extends Product with Serializable
         |${indent(0)(caseClasses: _*)}
         |
         |""".stripMargin + lineSeparator
    }

    s"""object Responses {
       |${indent(2, separator = doubleLineSeparator)(responses: _*)}
       |}""".stripMargin
  }

  def renderClientResponsesObject(api: Api) = {
    val responses = for {
      operation <- api.paths
    } yield {
      val adtTopName = responseAdtTopName(operation)
      val standardFields = Vector(CaseClassFieldDescription("responseData", "ResponseData[F]"))

      val unexpectedResponseCaseClass: Option[String] = Option.when(
        !operation.responses.exists(_.httpCode == Default)
      )(
        renderOtherResponseCaseClass(adtTopName, standardFields)
      )

      val caseClasses = operation.responses
        .map(renderResponseCaseClass(adtTopName, standardFields, Some("[F[_]]")))
        .appendedAll(unexpectedResponseCaseClass)

      s"""sealed trait $adtTopName extends Product with Serializable
         |${indent(0)(caseClasses: _*)}
         |
         |""".stripMargin + lineSeparator
    }

    s"""object Responses {
       |  case class ResponseData[F[_]](
       |    raw: Response[F]
       |  ) {
       |    def headers: Headers = raw.headers
       |  }
       |
       |${indent(2, separator = doubleLineSeparator)(responses: _*)}
       |}""".stripMargin
  }

  private def renderResponseCaseClass(adtTopName: String, standardFields: Vector[CaseClassFieldDescription], suffix: Option[String] = None)(response: Response): String = {
    val caseClassName = responseCaseClassName(adtTopName, Some(response.httpCode), suffix)

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
      typeNameForResponseBody(response.body).map { typeDefinition =>
        CaseClassFieldDescription("content", typeDefinition)
      },
      standardFields
    )

    ScalaSpecifics.Rendering.renderCaseClass(
      caseClassName,
      responseFields,
      classAccessModifier = Some("final"),
      extendsClasses = Seq(adtTopName)
    )
  }
  private def renderOtherResponseCaseClass(adtTopName: String, standardFields: Vector[CaseClassFieldDescription]): String = {
    val otherResponseFields = Vector(
      CaseClassFieldDescription("httpCode", "Int"),
      CaseClassFieldDescription("content", "EntityBody[F]"),
    )
    ScalaSpecifics.Rendering.renderCaseClass(
      responseCaseClassName(adtTopName, None),
      otherResponseFields ++ standardFields,
      classAccessModifier = Some("final"),
      extendsClasses = Seq(adtTopName),
      effect = Some("[F[_]]")
    )
  }


}
