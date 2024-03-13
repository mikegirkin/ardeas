package net.girkin.ardeas.parser

import cats.*
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Kleisli, NonEmptyChain, NonEmptyList, OptionT, Reader, ValidatedNec}
import cats.data.given
import cats.implicits.given
import cats.syntax.either.*
import io.swagger.v3.oas.models.media.{ArraySchema, BooleanSchema, ComposedSchema, DateSchema, DateTimeSchema, IntegerSchema, MapSchema, NumberSchema, ObjectSchema, Schema, StringSchema, UUIDSchema}
import io.swagger.v3.oas.models.parameters.Parameter
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{Operation, PathItem}
import io.swagger.v3.parser.OpenAPIV3Parser
import io.swagger.v3.parser.core.models.{ParseOptions, SwaggerParseResult}
import net.girkin.ardeas.Model.Parameter.{CookieParameter, HeaderParameter, PathParameter, QueryParameter}
import net.girkin.ardeas.Model.PathSegment.{StringSegment, TemplatedParameter}
import net.girkin.ardeas.Model.Schema.HMap
import net.girkin.ardeas.Model.{HttpCode, HttpOperation, HttpVerb, InnerSchema, NamedSchemaRef, NonAnonymousObjectSchema, RequestBody, RequestParameterLocation, ResponseBody, SchemaOrRef}
import net.girkin.ardeas.parser.{ParsingError, ParsingLocation as PL}
import net.girkin.ardeas.parser.ParsingError.HttpOperationParsingError
import net.girkin.ardeas.{Logging, Model, ParserKleisliUtils}
import io.swagger.v3.oas.models as oapiModels
import net.girkin.ardeas.parser.ParsingLocation.SchemaLocation

import java.net.URI
import _root_.scala.jdk.CollectionConverters.*
import scala.collection.immutable.ListMap

object Parser extends Logging:

  def parse(openapiFileURL: URI): ValidatedNec[ParsingError, Model.Api] = {
    val parseResult: SwaggerParseResult = new OpenAPIV3Parser().readLocation(openapiFileURL.toString, List.empty.asJava, new ParseOptions)
    val api = parseResult.getOpenAPI

    val unparsedNamedSchemasMap = Option(api.getComponents)
      .flatMap(c => Option(c.getSchemas.asScala))
      // ListMap - to keep the order defined in the yaml file
      .map(schemaMap => ListMap.from(schemaMap))
      .getOrElse(Map.empty)
    val knownSchemaNames = unparsedNamedSchemasMap.keys.toList
    val namedSchemasV = parseNamedSchemas(unparsedNamedSchemasMap)

    val unparsedNamedRequestBodiesMap = Option(api.getComponents)
      .flatMap(c => Option(c.getRequestBodies.asScala))
      .map(_.toMap)
      .getOrElse(Map.empty)
    val knownRequestBodiesNames = unparsedNamedRequestBodiesMap.keys.toList
    val namedRequestBodiesV = parseComponentsRequestBodies(knownSchemaNames, unparsedNamedRequestBodiesMap)

    val unparsedNamedResponses = Option(api.getComponents)
      .flatMap(c => Option(c.getResponses.asScala))
      .map(_.toMap)
      .getOrElse(Map.empty)
    val knownResponseNames = unparsedNamedResponses.keys.toList
    val namedResponsesV = parseComponentsResponses(knownSchemaNames, unparsedNamedResponses)

    val pathsV = parsePaths(knownSchemaNames, knownRequestBodiesNames, knownResponseNames, api.getPaths.asScala.toMap)
    (pathsV, namedSchemasV, namedRequestBodiesV, namedResponsesV).mapN { (paths, schemas, namedRequestBodies, namedResponses) =>
      Model.Api(
        paths = paths,
        schemas = schemas,
        namedRequestBodies = namedRequestBodies,
        namedResponses = namedResponses
      )
    }
  }

  def parsePaths(knownSchemaNames: List[String], knownNamedRequestBodies: List[String], knownNamedResponses: List[String], paths: Map[String, PathItem]): ValidatedNec[ParsingError, Vector[Model.HttpOperation]] =
    paths.map { (pathStr, pathItem) =>
      val result = for {
        parsedPath <- parsePath(pathStr).toEither
        parsedItem <- parsePathItem(knownSchemaNames, knownNamedRequestBodies, knownNamedResponses, parsedPath, pathItem).toEither
      } yield {
        parsedItem
      }
      result.toValidated
    }.toList.combineAll

  def parsePathItem(knownSchemaNames: List[String], knownNamedRequestBodies: List[String], knownNamedResponses: List[String], path: Model.Path, pathItem: PathItem): ValidatedNec[ParsingError, Vector[Model.HttpOperation]] =
    import Model.HttpVerb.*
    Vector(
      Get -> Option(pathItem.getGet),
      Post -> Option(pathItem.getPost),
      Put -> Option(pathItem.getPut),
      Patch -> Option(pathItem.getPatch),
      Delete -> Option(pathItem.getDelete),
      Head -> Option(pathItem.getHead),
      Options -> Option(pathItem.getOptions),
      Trace -> Option(pathItem.getTrace),
    ).collect {
      case (httpVerb, Some(operation)) => parseOperation(knownSchemaNames, knownNamedRequestBodies, knownNamedResponses, path, httpVerb, operation).map(op => Vector(op))
    }.combineAll

  private def parseInlineRequestBody(location: PL.RequestBodyLocation, knownSchemaNames: List[String]): Kleisli[Option, io.swagger.v3.oas.models.parameters.RequestBody, ValidatedNec[ParsingError, RequestBody.Definition]] = {
    Kleisli { oapiRequestBody =>
      val schemaOptV: Option[ValidatedNec[ParsingError, Model.NonAnonymousObjectSchema]] = for {
        content <- Option(oapiRequestBody.getContent)
        jsonDefinition <- Option(content.get("application/json"))
        schema <- Option(jsonDefinition.getSchema)
      } yield {
        val schemaParsingLocation = PL.RequestBody(location)
        parseAsNonAnonymousObjectSchema(schemaParsingLocation, knownSchemaNames)(schema).getOrElse(
          ParsingError.SchemaParsingError(schemaParsingLocation, "Could not parse inner object in a request body").invalidNec
        )
      }
      val required = Option(oapiRequestBody.getRequired).map(_.booleanValue()).getOrElse(false)
      val schemaVOpt: ValidatedNec[ParsingError, Option[Model.NonAnonymousObjectSchema]] = schemaOptV.sequence
      val result: ValidatedNec[ParsingError, RequestBody.Definition] = schemaVOpt.map { schemaOpt =>
        RequestBody.Definition(required, schemaOpt)
      }
      Some(result)
    }
  }

  private def parseNamedRef[T](knownNamedItems: List[String], prefix: String, constructor: String => T)(refText: String): ValidatedNec[ParsingError, T] = {
    if (refText.startsWith(prefix)) {
      val typeName = refText.drop(prefix.length)
      if (knownNamedItems.contains(typeName)) {
        constructor(typeName).validNec
      } else {
        ParsingError.RefUnresolvable(refText).invalidNec
      }
    } else {
      ParsingError.RefStringUnparsable(refText).invalidNec
    }
  }

  private def parseNamedRequestBodyRef(knownNamedRequestBodies: List[String]): Kleisli[Option, io.swagger.v3.oas.models.parameters.RequestBody, ValidatedNec[ParsingError, RequestBody.NamedRef]] = {
    Kleisli { oapiRequestBody =>
      Option(oapiRequestBody.get$ref()).map {
        parseNamedRef(knownNamedRequestBodies, Constants.Components.RequestBodiesRef, RequestBody.NamedRef.apply)
      }
    }
  }

  private def parseRequestBody(location: PL.RequestBodyLocation, knownSchemaNames: List[String], knownNamedRequestBodies: List[String]): Kleisli[Option, io.swagger.v3.oas.models.parameters.RequestBody, ValidatedNec[ParsingError, RequestBody]] = {
    import ParserKleisliUtils.*

    parseNamedRequestBodyRef(knownNamedRequestBodies) orTryParseWith parseInlineRequestBody(location, knownSchemaNames)
  }

  def parseOperation(
    knownSchemaNames: List[String],
    knownNamedRequestBodies: List[String],
    knownNamedResponses: List[String],
    path: Model.Path,
    verb: HttpVerb,
    operation: Operation
  ): ValidatedNec[ParsingError, Model.HttpOperation] = {
    import ParserKleisliUtils.*

    val currentLocation = PL.Operation(path, verb)
    val unparsedParameters = Option(operation.getParameters).map(_.asScala.toVector).getOrElse(Vector.empty)
    val parametersV = parseOperationParameters(currentLocation, knownSchemaNames, unparsedParameters)
    val unparsedResponses = Option(operation.getResponses).map(_.asScala.toVector).getOrElse(Vector.empty)
    val operationResponsesV = parseOperationResponses(currentLocation, knownSchemaNames, knownNamedResponses, unparsedResponses)
    val requestBodyV = Option(operation.getRequestBody).map {
      parseRequestBody(currentLocation, knownSchemaNames, knownNamedRequestBodies)
        .closeWithError(ParsingError.HttpOperationParsingError(path, verb, "Can't parse request body"))
        .run
    }.sequence

    (parametersV, operationResponsesV, requestBodyV).mapN { case (parameters, responses, requestBody) =>
      HttpOperation(
        path,
        verb,
        Option(operation.getOperationId),
        parameters,
        responses,
        requestBody
      )
    }
  }

  private def parsePath(path: String): ValidatedNec[ParsingError.HttpPathParsingError, Model.Path] = {
    path.splitAt(1) match {
      case ("/", otherPath) =>
        val segments = otherPath.split("/").toList
        val parsedSegments = segments.map { segment =>
          if(segment.startsWith("{") && segment.endsWith("}")) {
            TemplatedParameter(segment.drop(1).dropRight(1))
          } else {
            StringSegment(segment)
          }
        }
        Model.Path(parsedSegments).validNec
      case _ =>
        ParsingError.HttpPathParsingError(path, "Path must start with \"/\"").invalidNec
    }
  }

  private def parseParameterLocation(locationStr: String): ValidatedNec[ParsingError, Model.RequestParameterLocation] = {
    locationStr.toLowerCase match {
      case "path" => Model.RequestParameterLocation.Path.validNec
      case "query" => Model.RequestParameterLocation.Query.validNec
      case "header" => Model.RequestParameterLocation.Header.validNec
      case "cookie" => Model.RequestParameterLocation.Cookie.validNec
      case _ => ParsingError.HttpParameterLocationParsingError(locationStr).invalidNec
    }
  }

  private def createParameter(name: String, in: RequestParameterLocation, schema: Model.NonAnonymousObjectSchema, required: Boolean): ValidatedNec[ParsingError, Model.Parameter] = {
    in match {
      case RequestParameterLocation.Query => {
        QueryParameter(name, schema, required).validNec
      }
      case RequestParameterLocation.Header => HeaderParameter(name, schema, required).validNec
      case RequestParameterLocation.Path =>
        (schema, required) match {
          case (stdType: Model.Schema.StandardType, true) => PathParameter(name, stdType).validNec
          case (_, false) => ParsingError.HttpParameterParsingError(name, "Optional path parameters are not supported").invalidNec
          case _ => ParsingError.HttpParameterParsingError(name, "Non-primitive schema in path parameters are not supported").invalidNec
        }
      case RequestParameterLocation.Cookie => CookieParameter(name, schema, required).validNec
    }
  }

  def parseOperationParameters(location: PL.Operation, knownSchemaNames: List[String], parameters: Vector[Parameter]): ValidatedNec[ParsingError, Vector[Model.Parameter]] = {
    val parameterParsingResult: Vector[ValidatedNec[ParsingError, Model.Parameter]] = parameters.map { p =>
      val parsedParameterLocationV = Option(p.getIn)
        .map(parseParameterLocation)
        .getOrElse(
          ParsingError.HttpParameterLocationParsingError(s"Empty location, parameter ${p.getName}")
            .invalidNec
        )

      val parsedSchemaV = parseAsNonAnonymousObjectSchema(PL.RequestParameter(location, p.getName), knownSchemaNames)(p.getSchema).getOrElse(
        ParsingError.HttpParameterParsingError(p.getName, "Anonymous objects in parameters are not allowed").invalidNec
      )

      val parsedNameV = Option(p.getName)
        .map(_.validNec)
        .getOrElse(ParsingError.HttpParameterParsingError(p.getName, "Parameter must have a non-null name").invalidNec)

      (parsedNameV, parsedParameterLocationV, parsedSchemaV).mapN {
        case (name, location, schema) => (name, location, schema, Option(p.getRequired).fold(false){  _.booleanValue() })
      }.andThen {
        createParameter.tupled
      }
    }

    parameterParsingResult.map(_.map(x => Vector(x))).combineAll
  }

  private def parseNamedResponseRef(knownNamedResponses: List[String]): Kleisli[Option, ApiResponse, ValidatedNec[ParsingError, Model.ResponseBody]] = {
    Kleisli { openapiResponse =>
      Option(openapiResponse.get$ref()).map {
        parseNamedRef(knownNamedResponses, Constants.Components.ResponsesRef, ResponseBody.NamedRef.apply)
      }
    }
  }

  private def parseResponseDefinition(responseLocation: PL.ResponseLocation, knownSchemaNames: List[String]): Reader[ApiResponse, ValidatedNec[ParsingError, Model.ResponseBody.Definition]] = {
    Reader { openapiResponse =>
      val schemaOptV = for {
        content <- Option(openapiResponse.getContent)
        json <- Option(content.get("application/json"))
        schema <- Option(json.getSchema)
      } yield {
        val schemaLocation = PL.Response(responseLocation)
        parseAsNonAnonymousObjectSchema(schemaLocation, knownSchemaNames)(schema).getOrElse(
          ParsingError.SchemaParsingError(schemaLocation, "Could not parse inner object in a response").invalidNec
        )
      }

      schemaOptV.sequence.map { schemaOpt =>
        ResponseBody.Definition(schemaOpt)
      }
    }
  }

  private def parseHttpCode(location: PL.Operation)(httpCodeStr: String) = {
    httpCodeStr.toLowerCase match {
      case "default" => Model.Default.validNec
      case value => value.toIntOption.toValidNec(
        ParsingError.HttpOperationResponseParsingError(location.path, location.verb, httpCodeStr, "Invalid http code")
      )
    }
  }

  private def parseOperationResponses(
    location: PL.Operation,
    knownSchemaNames: List[String],
    knownResponseNames: List[String],
    responses: Vector[(String, ApiResponse)]
  ): ValidatedNec[ParsingError, Vector[Model.Response]] = {
    import ParserKleisliUtils.*

    val responsesV: Vector[ValidatedNec[ParsingError, Model.Response]] =
      responses.map { case (httpCodeStr, response) =>
        val httpCodeV = parseHttpCode(location)(httpCodeStr)

        val responseBodyLocation = PL.OperationResponse(location, httpCodeStr)
        val parsedResponseBodyV = parseNamedResponseRef(knownResponseNames)(response)
          .getOrElse(parseResponseDefinition(responseBodyLocation, knownSchemaNames)(response))

        (httpCodeV, parsedResponseBodyV).mapN { case (httpCode, responseBody) =>
          Model.Response(
            httpCode,
            responseBody
          )
        }
      }
    responsesV.map(itemV => itemV.map(item => Vector(item))).combineAll
  }

  private def parseSchema(schemaLocation: PL.SchemaLocation, knownSchemaNames: List[String], schema: Schema[_]): ValidatedNec[ParsingError, SchemaOrRef] = {
    import ParserKleisliUtils.*

    (parseStdTypeSchema orTryParseWith
      parseObjectSchema(schemaLocation, knownSchemaNames) orTryParseWith
      parseNamedSchemaRef(knownSchemaNames) orTryParseWith
      parseArraySchema(schemaLocation, knownSchemaNames) orTryParseWith
      parseOneOfSchema(schemaLocation, knownSchemaNames))
      .closeWithError(ParsingError.SchemaParsingError(schemaLocation, "Unknown schema"))
      .map[SchemaOrRef](identity)
      .apply(schema)
  }

  type PartialParser[OapiObject, OutObject] = Kleisli[Option, OapiObject, ValidatedNec[ParsingError, OutObject]]
  type PartialSchemaParser[OutSchema] = Kleisli[Option, Schema[_], ValidatedNec[ParsingError, OutSchema]]
  object PartialSchemaParser {
    def fromPartialFunction[R](pf: PartialFunction[Schema[_], ValidatedNec[ParsingError, R]]): PartialSchemaParser[R] = {
      Kleisli(pf.lift)
    }

    def fromPartialOptionalFunction[R](pf: PartialFunction[Schema[_], Option[ValidatedNec[ParsingError, R]]]): PartialSchemaParser[R] = {
      Kleisli {
        pf.andThen {
          case Some(x) => x
        }.lift
      }
    }
  }

  private val parseStdTypeSchema: PartialSchemaParser[Model.Schema.StandardType] = PartialSchemaParser.fromPartialFunction {
    case stdType: OpenApiStandardTypeSchema =>
      val parsedStdType = Model.Schema.StandardType(stdType.getType, Option(stdType.getFormat))
      parsedStdType.validNec
  }

  private def parseNamedSchemaRef(knownSchemaNames: List[String]): PartialSchemaParser[Model.NamedSchemaRef] = Kleisli { schema =>
    Option(schema.get$ref()).map(
      parseNamedRef(knownSchemaNames, Constants.Components.SchemasRef, NamedSchemaRef.apply)
    )
  }

  private def parseArraySchema(schemaLocation: PL.SchemaLocation, knownSchemaNames: List[String]): PartialSchemaParser[Model.Schema.Array] = PartialSchemaParser.fromPartialFunction {
    case arr: ArraySchema =>
      val innerSchemaV: ValidatedNec[ParsingError, InnerSchema] = parseAsInnerSchema(schemaLocation, knownSchemaNames)(arr.getItems).getOrElse {
        ParsingError.SchemaParsingError(schemaLocation, "Could not parse anonymous object in array").invalidNec
      }
      innerSchemaV.map { Model.Schema.Array.apply }
  }

  private def parseObjectAsHMap(schemaLocation: PL.SchemaLocation, knownSchemaNames: List[String]): PartialSchemaParser[Model.Schema.HMap] = PartialSchemaParser.fromPartialOptionalFunction {
    case obj: MapSchema =>
      val additionalProperties: Option[Boolean | Schema[_]] = Option(obj.getAdditionalProperties).map {
        case b: java.lang.Boolean => b.booleanValue()
        case s: Schema[_] => s
      }
      val isPropertiesPresent = Option(obj.getProperties).map(_.asScala.nonEmpty).getOrElse(false)
      if(!isPropertiesPresent) {
        additionalProperties.flatMap {
          case true => Some(Model.Schema.HMap(Model.Schema.StandardType("String", None)).validNec)
          case s: Schema[_] => parseAsInnerSchema(schemaLocation, knownSchemaNames)(s).map(_.map(HMap.apply))
          case _ => None
        }
      } else {
        Some(ParsingError.SchemaParsingError(schemaLocation, "Object contains simultaneously properties and additionalProperties").invalidNec)
      }
  }

  private def oapiDiscriminatorToModel(schemaLocation: PL.SchemaLocation, knownSchemaNames: List[String], oapiDiscriminator: oapiModels.media.Discriminator): ValidatedNec[ParsingError, Model.Discriminator] = {
    val propertyNameV = Option(oapiDiscriminator.getPropertyName).fold(
      ParsingError.SchemaParsingError(schemaLocation, "discriminator can not be empty").invalidNec
    ) { str =>
      str.validNec
    }
    val mappingV = Option(oapiDiscriminator.getMapping)
      .map(_.asScala.toMap)
      .getOrElse(Map.empty)
      .map { case (name, schemaRef) =>
        parseNamedRef(knownSchemaNames, Constants.Components.SchemasRef, NamedSchemaRef.apply)(schemaRef).map { parsedRef =>
          Vector(name -> parsedRef)
        }
      }
      .toList
      .combineAll

    (propertyNameV, mappingV).mapN { case (propertyName, mapping) =>
      Model.Discriminator(propertyName, mapping.toMap)
    }
  }

  private def parseOneOfSchema(schemaLocation: PL.SchemaLocation, knownSchemaNames: List[String]): PartialSchemaParser[Model.Schema.OneOf] = PartialSchemaParser.fromPartialFunction {
    case schema: ComposedSchema =>
      val oneOfList = Option(schema.getOneOf).map(_.asScala.toList)
      oneOfList.fold(
        ParsingError.SchemaParsingError(schemaLocation, "Anything beyond OneOf schemas with references is not supported yet").invalidNec
      ) {
        case Nil => ParsingError.SchemaParsingError(schemaLocation, "Anything beyond OneOf schemas with references is not supported yet").invalidNec
        case head :: tail =>
          val discriminatorV = Option(schema.getDiscriminator).map {
            d => oapiDiscriminatorToModel(schemaLocation, knownSchemaNames, d)
          }.sequence
          val parsedRefsV = NonEmptyList(head, tail).map { ref =>
            parseNamedSchemaRef(knownSchemaNames)(ref).getOrElse(ParsingError.SchemaParsingError(schemaLocation, "").invalidNec)
          }.sequence

          (discriminatorV, parsedRefsV).mapN { (discriminator, parsedRefs) =>
            Model.Schema.OneOf(parsedRefs, discriminator)
          }
      }

  }

  private def parseAsNonAnonymousObjectSchema(schemaLocation: PL.SchemaLocation, knownSchemaNames: List[String]): PartialSchemaParser[NonAnonymousObjectSchema] = {
    import ParserKleisliUtils.orTryParseWith

    parseStdTypeSchema orTryParseWith parseNamedSchemaRef(knownSchemaNames) orTryParseWith parseArraySchema(schemaLocation, knownSchemaNames) orTryParseWith parseObjectAsHMap(schemaLocation, knownSchemaNames)
  }

  private def parseAsInnerSchema(schemaLocation: PL.SchemaLocation, knownSchemaNames: List[String]): PartialSchemaParser[InnerSchema] = {
    import ParserKleisliUtils.orTryParseWith

    parseStdTypeSchema orTryParseWith parseNamedSchemaRef(knownSchemaNames) orTryParseWith parseArraySchema(schemaLocation, knownSchemaNames)
  }

  private def parseObjectAsObject(schemaLocation: PL.SchemaLocation, knownSchemaNames: List[String]): PartialSchemaParser[Model.Schema.Object] = PartialSchemaParser.fromPartialOptionalFunction {
    case obj: ObjectSchema =>
      val propertiesOpt = Option(obj.getProperties).map(_.asScala.toVector).flatMap(properties => Option.when(properties.nonEmpty)(properties))
      val objectSchemaOptV = propertiesOpt.map { properties =>
        val fieldsParsed: Vector[ValidatedNec[ParsingError, Model.EntityField]] = properties.map { case (fieldName, propertySchema) =>
          parseAsInnerSchema(schemaLocation, knownSchemaNames)(propertySchema).getOrElse {
            ParsingError.SchemaParsingError(schemaLocation, "Could not parse anonymous object in object").invalidNec
          }.map { parsedFieldSchema =>
            Model.EntityField(
              fieldName,
              parsedFieldSchema,
              Option(obj.getRequired).exists(_.contains(fieldName))
            )
          }
        }
        val fieldsV: ValidatedNec[ParsingError, Vector[Model.EntityField]] = fieldsParsed.map(itemV => itemV.map(item => Vector(item))).combineAll
        fieldsV.map { fields =>
          Model.Schema.Object(fields)
        }
      }
      objectSchemaOptV
  }

  private def parseObjectSchema(schemaLocation: PL.SchemaLocation, knownSchemaNames: List[String]): PartialSchemaParser[Model.Schema.Object | Model.Schema.HMap] = {
    import ParserKleisliUtils.orTryParseWith

    parseObjectAsObject(schemaLocation, knownSchemaNames) orTryParseWith parseObjectAsHMap(schemaLocation, knownSchemaNames)
  }

  def parseNamedSchemas(unparsedSchemas: Map[String, Schema[_]]): ValidatedNec[ParsingError, Map[String, SchemaOrRef]] = {
    val knownSchemaNames = unparsedSchemas.keys.toList
    unparsedSchemas.map {
      case ((schemaName, schema)) => parseSchema(PL.NamedStandalone(schemaName), knownSchemaNames, schema).map(schema => schemaName -> schema)
    }.map(
      itemV => itemV.map(item => Vector(item))
    ).toList
      .combineAll
      .map(x => _root_.scala.collection.immutable.ListMap(x:_*))
  }

  def parseComponentsRequestBodies(knownSchemaNames: List[String], requestBodies: Map[String, oapiModels.parameters.RequestBody]): ValidatedNec[ParsingError, Map[String, Model.RequestBody.Definition]] = {
    import ParserKleisliUtils.*

    requestBodies.map {
      case ((requestBodyName, requestBody)) => {
        parseInlineRequestBody(PL.NamedStandalone(requestBodyName), knownSchemaNames)
          .closeWithError(ParsingError.UnsupportedRequestBody(PL.NamedStandalone(requestBodyName), "Failed to parse named response body "))
          .run(requestBody)
          .map { requestBody => requestBodyName -> requestBody }
      }
    }.map {
      itemV => itemV.map(item => Vector(item))
    }.toList
      .combineAll
      .map(_.toMap)
  }

  def parseComponentsResponses(knownSchemaNames: List[String], responses: Map[String, oapiModels.responses.ApiResponse]): ValidatedNec[ParsingError, Map[String, Model.ResponseBody.Definition]] = {
    responses.map {
      case (responseName, response) => {
        parseResponseDefinition(PL.NamedStandalone(responseName), knownSchemaNames)
          .run(response)
          .map { responseBody => responseName -> responseBody }
      }
    }.map {
      itemV => itemV.map(item => Vector(item))
    }.toList
      .combineAll
      .map(_.toMap)
  }

  type OpenApiStandardTypeSchema =
    IntegerSchema | BooleanSchema | NumberSchema | StringSchema | NumberSchema | DateSchema | DateTimeSchema | UUIDSchema
