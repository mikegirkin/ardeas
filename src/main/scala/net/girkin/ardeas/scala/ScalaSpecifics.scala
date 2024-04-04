package net.girkin.ardeas.scala

import cats.data.ValidatedNec
import net.girkin.ardeas.ValidatedUtils.*
import net.girkin.ardeas.{Logging, Model}
import net.girkin.ardeas.Model._
import net.girkin.ardeas.RenderUtils.*

case class NotYetImplemented(
  message: String
)

object ScalaSpecifics extends Logging {
  import Model.Schema.StandardType

  object KnownOpenApiTypes {
    val Integer = "integer"
    val `String` = "string"
    val Number = "number"
    val `Boolean` = "boolean"
  }

  case class CouldNotConvertType(openApiType: StandardType)

  def openApiPrimitiveTypeToScala(openApiType: StandardType): Either[CouldNotConvertType, String] = {
    val typeMatcher: PartialFunction[StandardType, String] =
      case StandardType(KnownOpenApiTypes.Integer, Some("int64")) => "Long"
      case StandardType(KnownOpenApiTypes.Integer, _) => "Int"
      case StandardType(KnownOpenApiTypes.Number, Some("float")) => "Float"
      case StandardType(KnownOpenApiTypes.Number, _) => "Double"
      case StandardType(KnownOpenApiTypes.`Boolean`, _) => "Boolean"
      case StandardType(KnownOpenApiTypes.`String`, Some("date")) => "java.time.LocalDate"
      case StandardType(KnownOpenApiTypes.`String`, Some("datetime")) => "java.time.ZonedDateTime"
      case StandardType(KnownOpenApiTypes.`String`, Some("uuid")) => "java.util.UUID"
      case StandardType(KnownOpenApiTypes.`String`, _) => "String"

    typeMatcher.lift(openApiType).toRight(CouldNotConvertType(openApiType))
  }

  def escapeName(name: String) = {
    s"`$name`"
  }

  object VariableNaming {

    private val reservedWords = Set(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "forSome",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "this",
      "throw",
      "trait",
      "true",
      "try",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield"
    )

    def isReservedWord(text: String) = {
      reservedWords.contains(text)
    }

    private def escapeIfReservedTerm(text: String) = {
      if (isReservedWord(text)) {
        escapeName(text)
      } else {
        text
      }
    }

    def variableName(text: String) = {
      (escapeIfReservedTerm andThen lowercaseFirst)(text)
    }
  }

  object MethodNaming {
    def methodNameForOperation(operation: HttpOperation): String = {
      operation.operationId.getOrElse {
        s"`${operation.verb}${operation.path}`"
      }
    }

    def responseAdtTopName(operation: HttpOperation): String = {
      capitalizeFirst(s"${methodNameForOperation(operation)}Response")
    }

    def methodDefinitionForOperation(
      operation: HttpOperation,
      pathVarTypesTranslator: StandardType => String,
      additionalParameters: Map[String, String] = Map.empty
    ): String = {
      val methodName = methodNameForOperation(operation)
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
        case ref @ Model.RequestBody.NamedRef(_) =>
          s"body: ${TypeNaming.typeNameFromReference(ref, useFullyQualifiedRef = true)}"

        case rb @ Model.RequestBody.Definition(_, _) =>
          val bodyParameterTypeName = rb.jsonContent.fold(
            "String"
          ) { schema =>
            ScalaSpecifics.TypeNaming.typeNameForRequestBody(schema)
          }
          val bodyParameterType = TypeNaming.typeForOptional(bodyParameterTypeName, rb.required)
          s"body: $bodyParameterType"
      }

      val queryParameterDefinitions = operation.parameters.collect {
        case Parameter.QueryParameter(parameterName, arrSchema @ Schema.Array(innerSchema), required) => {
          parameterName -> TypeNaming.typeDefinitionForArray(arrSchema)
        }
        case Parameter.QueryParameter(parameterName, schema, required) => {
          parameterName -> TypeNaming.typeForOptional(TypeNaming.typeNameForNonAnonymousObjectSchema(schema), required)
        }
      }.map { case (parameterName, typeDefinition) =>
        s"$parameterName: $typeDefinition"
      }

      val headerParameter = s"headers: Headers = Headers.empty"
      val additionalParameterDefinitions = additionalParameters.map { case (name, typeName) => s"$name: $typeName" }
      val parameterDefinitions = Vector.concat(
        additionalParameterDefinitions,
        pathParameterDefinitions,
        bodyParameter,
        queryParameterDefinitions,
        List(headerParameter)
      )
      val returnType = responseAdtTopName(operation)
      s"def $methodName(${parameterDefinitions.mkString(", ")}): F[${returnType}]"
    }
  }

  object TypeNaming:

    def typeDefinitionForArray(schema: Schema.Array, useFullyQualified: Boolean = false): String =
      val innerTypeDefinition = typeNameForNonAnonymousObjectSchema(schema.itemSchema, useFullyQualified = useFullyQualified)
      s"Vector[${innerTypeDefinition}]"

    def typeNameFromStandardType(std: Model.Schema.StandardType): String =
      ScalaSpecifics.openApiPrimitiveTypeToScala(std).getOrElse {
        val msg = s"Don't know matching type for ${std}"
        logger.error(msg)
        throw new Exception(msg)
      }

    def typeNameFromReference(ref: NamedSchemaRef, useFullyQualifiedRef: Boolean = false): String = {
      val prefix = if (useFullyQualifiedRef) "Components.Schemas." else ""
      s"${prefix}${ref.name}"
    }
    def typeNameFromReference(ref: RequestBody.NamedRef, useFullyQualifiedRef: Boolean): String = {
      val prefix = if (useFullyQualifiedRef) "Components.RequestBodies." else ""
      s"${prefix}${ref.name}"
    }
    def typeNameFromReference(ref: ResponseBody.NamedRef, useFullyQualifiedRef: Boolean): String = {
      val prefix = if (useFullyQualifiedRef) "Components.Responses." else ""
      s"${prefix}${ref.name}"
    }

    def typeDefinitionForHmap(hmapSchema: Schema.HMap): String = {
      val elementTypeDefinition = typeNameForNonAnonymousObjectSchema(hmapSchema.itemSchema)
      s"Map[String, $elementTypeDefinition]"
    }

    def typeNameForNonAnonymousObjectSchema(schema: NonAnonymousObjectSchema, required: Boolean = true, useFullyQualified: Boolean = false): String = {
      val typeName = schema match {
        case hmap: Model.Schema.HMap =>
          typeDefinitionForHmap(hmap)
        case arr: Model.Schema.Array =>
          typeDefinitionForArray(arr)
        case ref: NamedSchemaRef =>
          typeNameFromReference(ref, useFullyQualified)
        case std: Model.Schema.StandardType =>
          typeNameFromStandardType(std)
      }
      typeForOptional(typeName, required)
    }

    def typeForOptional(typeName: String, required: Boolean): String = {
      if (required) {
        typeName
      } else {
        s"Option[$typeName]"
      }
    }

    def typeNameForField(field: EntityField): String = {
      typeNameForNonAnonymousObjectSchema(field.schema, field.required)
    }

    def typeNameForRequestBody(schema: NonAnonymousObjectSchema): String = {
      typeNameForNonAnonymousObjectSchema(schema)
    }

    def typeNameForResponseBody(responseBody: ResponseBody): Option[String] = {
      responseBody match
        case ResponseBody.Definition(jsonContent) => jsonContent.map(schema => typeNameForNonAnonymousObjectSchema(schema, required = true))
        case ref @ ResponseBody.NamedRef(_) => Some(typeNameFromReference(ref, useFullyQualifiedRef = true))
    }

  object Rendering {
    final case class CaseClassFieldDescription(name: String, typeDefinition: String)

    def renderCaseClass(
      typeName: String,
      fields: Seq[CaseClassFieldDescription],
      classAccessModifier: Option[String] = None,
      extendsClasses: Seq[String] = Seq.empty,
      methodsBody: Option[String] = None,
      effect: Option[String] = None
    ) = {
      val fieldLines = fields.map { field =>
          s"${VariableNaming.variableName(field.name)}: ${field.typeDefinition}"
        }.mkString("," + lineSeparator)

      val prefix = classAccessModifier.map(_ + " ").getOrElse("")
      val extendsClause = extendsClasses.mkStringIfNonEmpty(
        " extends ",
        " with ",
        ""
      )
      val methodsClause = methodsBody.map { methods =>
        s""" {
           |${indent(2)(methods)}
           |}""".stripMargin
      }.getOrElse("")
      val suffix = s"${extendsClause}${methodsClause}"
      val renderedEffect = effect.getOrElse("")

      s"""${prefix}case class ${typeName}${renderedEffect}(
         |${indent(2)(fieldLines)}
         |)${suffix}""".stripMargin
    }
  }

}
