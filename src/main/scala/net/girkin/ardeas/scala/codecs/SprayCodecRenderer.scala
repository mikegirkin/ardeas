package net.girkin.ardeas.scala.codecs

import cats.data.ValidatedNec
import net.girkin.ardeas.Model.{EntityField, NamedSchemaRef}
import net.girkin.ardeas.{Logging, Model, Render}
import net.girkin.ardeas.scala.{NotYetImplemented, ScalaSpecifics}
import net.girkin.ardeas.ValidatedUtils.*

object SprayCodecRenderer extends CodecRenderer with Render with Logging:

  override def renderCodecsForModels(api: Model.Api, `package`: Option[String], additionalImports: Iterable[String]): ValidatedNec[NotYetImplemented, String] = {
    validNec {
      val prefix = packageAndImportsHeader(`package`, additionalImports).map { header =>
        header + doubleLineSeparator
      }.getOrElse("")

      val innerText = api.schemas.collect {
        case (name, obj: Model.Schema.Object) => renderCodecFor(name, obj)
      }.mkString(System.lineSeparator())

      s"""${prefix}object Codecs {
         |${indent(2)(innerText)}
         |}""".stripMargin
    }
  }

  def renderCodecFor(typeName: String, entity: Model.Schema.Object): String =
    s"""implicit val ${typeName}Format: RootJsonFormat[${typeName}] = new RootJsonFormat[${typeName}] {
       |${indent(2)(renderWriter(typeName, entity))}
       |${indent(2)(renderReader(typeName, entity))}
       |}""".stripMargin

  def renderWriter(typeName: String, entity: Model.Schema.Object): String =
    val propertyEncodeLines =
      entity.fields.map(propertyDecodeLine).mkString("," + lineSeparator)
    s"""override def write(obj: ${typeName}): JsValue = JsObject(
       |${indent(2)(propertyEncodeLines)}
       |)""".stripMargin

  def renderReader(typeName: String, entity: Model.Schema.Object): String =
    val fieldNames = entity.fields.map(_.name)
    val jsonFieldNames = fieldNames
      .map(name => s"\"${name}\"")
      .mkString(", ")
    val scalaFieldNames = fieldNames.mkString(", ")
    val fieldConverters = entity.fields
      .map { field =>
        val fieldType = ScalaSpecifics.TypeNaming.typeNameForField(typeName)(field)
        fieldConverter(fieldType)(field)
      }.mkString("," + lineSeparator)

    s"""override def read(json: JsValue): ${typeName} = {
      |  json.asJsObject.getFields(${jsonFieldNames}) match {
      |    case Seq(${scalaFieldNames}) =>
      |      ${typeName}(
      |${indent(8)(fieldConverters)}
      |      )
      |    case _ => throw DeserializationException(s"Could not deserialize $${json} to ${typeName}")
      |  }
      |}""".stripMargin

  def propertyDecodeLine(entityField: EntityField): String =
    s"\"${entityField.name}\" -> ${encoderFor(entityField)}"

  def encoderFor(entityField: EntityField): String =
    entityField.schema match {
      case NamedSchemaRef(_) |
           Model.Schema.Array(_) =>
        s"obj.${entityField.name}.toJson"
      case st : Model.Schema.StandardType =>
        encoderForStandardType(st, s"obj.${entityField.name}")
    }

  def encoderForStandardType(stdType: Model.Schema.StandardType, propertyExtractor: String): String =
    val sprayJsType = sprayJsTypeFor(stdType)
    val propertyWriterSuffix = if stdType.`type` == "string" then ".toString" else ""
    s"${sprayJsType}(${propertyExtractor}${propertyWriterSuffix})"

  def sprayJsTypeFor(stdType: Model.Schema.StandardType): String =
    stdType.`type` match {
      case "integer" |
           "number" => s"JsNumber"
      case "string" => s"JsString"
      case "boolean" => s"JsBoolean"
      case _ => s"JsString"
    }

  def fieldConverter(typeName: String)(field: EntityField) =
    s"${field.name}.convertTo[${typeName}]"
