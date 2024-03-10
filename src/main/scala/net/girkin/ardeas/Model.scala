package net.girkin.ardeas

import cats.data.NonEmptyList
import io.swagger.v3.oas.models.Components
import net.girkin.ardeas.Model.PathSegment.StringSegment

object Model:

  final case class Api(
    paths: Vector[HttpOperation],
    schemas: Map[String, SchemaOrRef],
    namedRequestBodies: Map[String, Model.RequestBody.Definition],
    namedResponses: Map[String, Model.ResponseBody.Definition]
  )

  object Api:
    def empty = new Api(
      Vector.empty,
      Map.empty,
      Map.empty,
      Map.empty
    )

  enum HttpVerb:
    case Get
    case Post
    case Put
    case Delete
    case Patch
    case Head
    case Trace
    case Options

  enum PathSegment:
    case StringSegment(str: String)
    case TemplatedParameter(name: String)

  final case class Path(segments: List[PathSegment])

  object Path {
    def of(segments: String*) = Path(segments.map { StringSegment.apply }.toList)
  }

  type SchemaOrRef = NamedSchemaRef | Schema
  type NonAnonymousObjectSchema = NamedSchemaRef | Schema.StandardType | Schema.Array | Schema.HMap
  type InnerSchema = NamedSchemaRef | Schema.StandardType | Schema.Array

  type RequestBody = RequestBody.Definition | RequestBody.NamedRef
  object RequestBody {
    final case class Definition(
      required: Boolean,
      jsonContent: Option[NonAnonymousObjectSchema]
    )

    final case class NamedRef(name: String)
  }

  enum ResponseBody {
    case Definition(
      jsonContent: Option[NonAnonymousObjectSchema]
    )
    case NamedRef(
      name: String
    )
  }

  case object Default {}

  type HttpCode = Int | Default.type

  final case class Response(
    httpCode: HttpCode,
    body: ResponseBody
  )

  final case class HttpOperation(
    path: Path,
    verb: HttpVerb,
    operationId: Option[String],
    parameters: Vector[Parameter],
    responses: Vector[Response],
    requestBody: Option[RequestBody]
  )

  final case class NamedSchemaRef(name: String)

  enum RequestParameterLocation:
    case Query
    case Header
    case Path
    case Cookie

  enum Parameter(val name: String, val in: RequestParameterLocation, val schema: NonAnonymousObjectSchema, val required: Boolean) {
    case QueryParameter(override val name: String, override val schema: NonAnonymousObjectSchema, override val required: Boolean) extends Parameter(name, RequestParameterLocation.Query, schema, required)
    case HeaderParameter(override val name: String, override val schema: NonAnonymousObjectSchema, override val required: Boolean) extends Parameter(name, RequestParameterLocation.Header, schema, required)
    case PathParameter(override val name: String, override val schema: Schema.StandardType) extends Parameter(name, RequestParameterLocation.Path, schema, true)
    case CookieParameter(override val name: String, override val schema: NonAnonymousObjectSchema, override val required: Boolean) extends Parameter(name, RequestParameterLocation.Cookie, schema, required)
  }

  final case class EntityField(
    name: String,
    schema: InnerSchema,
    required: Boolean
  )

  final case class Discriminator(
    propertyName: String,
    mapping: Option[Map[String, String]]
  )

  sealed trait Schema
  object Schema {
    final case class Object(
      fields: Vector[EntityField]
    ) extends Schema

    final case class Array(
      itemSchema: InnerSchema
    ) extends Schema

    final case class StandardType(
      `type`: String,
      format: Option[String] = None
    ) extends Schema

    final case class HMap(
      itemSchema: InnerSchema
    ) extends Schema

    final case class OneOf(
      schemas: NonEmptyList[NamedSchemaRef],
      discriminator: Option[Discriminator]
    ) extends Schema

    def makeObject(fields: EntityField*): Object = Object(fields.toVector)
  }
