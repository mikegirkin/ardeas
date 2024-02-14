package net.girkin.ardeas.parser

import cats.*
import cats.data.{State, ValidatedNec}
import net.girkin.ardeas.Model.{RequestBody, Response, *}

object ParsingLocation {
  final case class NamedStandalone(name: String)
  final case class Schema(location: SchemaLocation)
  final case class Response(location: ResponseLocation)
  final case class RequestBody(location: RequestBodyLocation)

  final case class Operation(path: Path, verb: HttpVerb)
  final case class RequestParameter(location: Operation, parameterName: String)
  final case class OperationResponse(operation: Operation, code: String)

  type SchemaLocation = NamedStandalone | Response | RequestBody | RequestParameter
  type ResponseLocation = NamedStandalone | OperationResponse
  type RequestBodyLocation = NamedStandalone | Operation
}

enum ParsingError {
  case SchemaParsingError(schema: ParsingLocation.SchemaLocation, message: String)
  case UnsupportedSchema(schema: ParsingLocation.SchemaLocation, message: String)
  case UnsupportedRequestBody(location: ParsingLocation.RequestBodyLocation, message: String)
  case UnsupportedResponseBody(location: ParsingLocation.ResponseLocation, message: String)
  case HttpOperationParsingError(path: Path, httpVerb: HttpVerb, message: String)
  case HttpOperationResponseParsingError(path: Path, httpVerb: HttpVerb, httpCode: String, message: String)
  case HttpPathParsingError(path: String, message: String)
  case HttpParameterLocationParsingError(location: String)
  case HttpParameterParsingError(parameter: String, message: String)
  case RefStringUnparsable(refString: String)
  case RefUnresolvable(refString: String)
}

