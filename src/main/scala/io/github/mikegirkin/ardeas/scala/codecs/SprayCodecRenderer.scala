package io.github.mikegirkin.ardeas.scala.codecs

import cats.data.ValidatedNec
import io.github.mikegirkin.ardeas.Model.{EntityField, NamedSchemaRef}
import io.github.mikegirkin.ardeas.{Logging, Model, Render}
import io.github.mikegirkin.ardeas.scala.{NotYetImplemented, ScalaSpecifics}
import io.github.mikegirkin.ardeas.ValidatedUtils.*

object SprayCodecRenderer extends CodecRenderer with Render with Logging:

  override def renderCodecsForModels(api: Model.Api, `package`: Option[String], additionalImports: Iterable[String]): ValidatedNec[NotYetImplemented, String] = {
    validNec {
      val requiredImports = Vector(
        "Components.Schemas._",
        "spray.json._",
        "spray.json.DefaultJsonProtocol._"
      )

      val prefix = packageAndImportsHeader(
        `package`,
        requiredImports.appendedAll(additionalImports)
      ).map { header =>
        header + doubleLineSeparator
      }.getOrElse("")

      val innerText = api.schemas.collect {
        case (name, obj: Model.Schema.Object) => renderCodecFor(name, obj)
        case (name, stringEnum: Model.Schema.StringEnum) => renderCodecForEnum(name, stringEnum)
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

  def renderCodecForEnum(typeName: String, stringEnum: Model.Schema.StringEnum): String = {
    val decoderLines = stringEnum.`enum`.map { line =>
      s"case \"${line}\" => ${typeName}.${line}"
    }
    val encoderLines = stringEnum.`enum`.map { line =>
      s"case ${typeName}.${line} => JsString(\"${line}\")"
    }

    s"""implicit val ${typeName}Format: RootJsonFormat[${typeName}] = new RootJsonFormat[Species] {
       |  override def write(obj: ${typeName}): JsValue = obj match {
       |${indent(4)(encoderLines: _*)}
       |  }
       |  override def read(json: JsValue): ${typeName} = {
       |    json.asJsString match {
       |${indent(6)(decoderLines: _*)}
       |    }
       |  }
       |}""".stripMargin
  }

  def renderWriter(typeName: String, entity: Model.Schema.Object): String =
    val propertyEncodeLines = entity.fields
      .map { entityField =>
        s"\"${entityField.name}\" -> obj.${ScalaSpecifics.VariableNaming.variableName(entityField.name)}.toJson"
      }.mkString("," + lineSeparator)

    s"""override def write(obj: ${typeName}): JsValue = JsObject(
       |${indent(2)(propertyEncodeLines)}
       |)""".stripMargin

  def renderReader(typeName: String, entity: Model.Schema.Object): String =
    val fieldNames = entity.fields.map(_.name)
    val jsonFieldNames = fieldNames
      .map(name => s"\"${name}\"")
      .mkString(", ")
    val scalaFieldNames = fieldNames.map { fieldName =>
      val variableName = ScalaSpecifics.VariableNaming.variableName(fieldName)
      if(ScalaSpecifics.isReservedWord(fieldName)) {
        s"${variableName} @ _"
      } else {
        variableName
      }
    }.mkString(", ")
    val fieldConverters = entity.fields
      .map { field =>
        val fieldType = ScalaSpecifics.TypeNaming.typeNameForField(field)
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

  def fieldConverter(typeName: String)(field: EntityField) =
    s"${ScalaSpecifics.VariableNaming.variableName(field.name)}.convertTo[${typeName}]"
