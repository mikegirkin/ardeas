package net.girkin.ardeas.scala.codecs

import cats.data.ValidatedNec
import cats.implicits.given
import net.girkin.ardeas.Model.Schema
import net.girkin.ardeas.RenderUtils.packageClause
import net.girkin.ardeas.scala.{NotYetImplemented, ScalaSpecifics}
import net.girkin.ardeas.{Logging, Model, Render}
import net.girkin.ardeas.ValidatedUtils.*

object Scala2CirceCodecRenderer extends CodecRenderer with Render with Logging {

  override def renderCodecsForModels(api: Model.Api, `package`: Option[String], additionalImportPackages: Iterable[String]): ValidatedNec[NotYetImplemented, String] = {
    val renderedCodecs: ValidatedNec[NotYetImplemented, Vector[String]] = api.schemas.collect {
      case (name, obj: Model.Schema.Object) => renderCodecFor(name, obj).map(item => Vector(item))
    }.toVector.combineAll

    val packageAndImportsPrefix = packageAndImportsHeader(
      `package`,
      Vector(
        "io.circe.{HCursor, Codec, Json}",
        "io.circe.syntax.EncoderOps",
        "Components.Schemas._"
      ).appendedAll(
        additionalImportPackages
      )
    ).getOrElse("")

    renderedCodecs.map(_.mkString(System.lineSeparator())).map { innerText =>
      s"""$packageAndImportsPrefix
         |
         |object Codecs {
         |${indent(2)(innerText)}
         |}""".stripMargin
    }
  }

  def renderCodecFor(typeName: String, schema: Model.Schema.Object): ValidatedNec[NotYetImplemented, String] = {
    val decoder = renderDecoder(typeName, schema)
    val encoderV = renderEncoder(typeName, schema)
    encoderV.map { encoder =>
      s"""implicit val ${typeName}Codec: Codec[${typeName}] = Codec.from(
         |${indent(2)(decoder)},
         |${indent(2)(encoder)}
         |)""".stripMargin
    }
  }

  def renderFieldDecoderLine(field: Model.EntityField): String = {
    val typeDefinition = ScalaSpecifics.TypeNaming.typeNameForField(field)
    s"""${ScalaSpecifics.VariableNaming.variableName(field.name)} <- c.downField("${field.name}").as[$typeDefinition]"""
  }

  def renderDecoder(typeName: String, schema: Model.Schema.Object): String = {
    val fieldLines: Vector[String] = schema.fields.map { field =>
      renderFieldDecoderLine(field)
    }
    val fieldNames = schema.fields.map(field => ScalaSpecifics.VariableNaming.variableName(field.name))
    val creationLine = s"""${typeName}(${fieldNames.mkString(", ")})"""
    s"""(c: HCursor) => {
       |${indent(2)("for {")}
       |${indent(4)(fieldLines:_*)}
       |${indent(2)("} yield {")}
       |${indent(4)(creationLine)}
       |${indent(2)("}")}
       |}""".stripMargin
  }

  def renderEncoder(typeName: String, schema: Model.Schema.Object): ValidatedNec[NotYetImplemented, String] = {
    val encoderLines = schema.fields
      .map { encoderLineForField }
      .mkString("," + lineSeparator)
    validNec(
      s"""(a: ${typeName}) => {
         |${indent(2)("Json.obj(")}
         |${indent(4)(encoderLines)}
         |${indent(2)(")")}
         |}""".stripMargin
    )
  }

  def encoderLineForField(field: Model.EntityField) = {
    s""""${field.name}" -> a.${ScalaSpecifics.VariableNaming.variableName(field.name)}.asJson"""
  }

  def stdTypeToEncoder(schema: Model.Schema.StandardType): String = {
    schema.`type` match {
      case "integer" if schema.format.contains("int64") => s"Json.fromLong"
      case "integer" => s"Json.fromInt"
      case "number" if schema.format.contains("float") => s"Json.fromFloat"
      case "number" if schema.format.contains("double") => s"Json.fromDouble"
      case "string" => s"Json.fromString"
      case "boolean" => s"Json.fromBoolean"
      case _ => s"Json.fromString"
    }
  }

}
