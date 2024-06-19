package net.girkin.ardeas.scala.codecs

import cats.data.{NonEmptyList, ValidatedNec}
import cats.implicits.given
import net.girkin.ardeas.Model.{Discriminator, NamedSchemaRef, Schema}
import net.girkin.ardeas.RenderUtils.packageClause
import net.girkin.ardeas.scala.{NotYetImplemented, ScalaSpecifics}
import net.girkin.ardeas.{Logging, Model, Render}
import net.girkin.ardeas.ValidatedUtils.*

object Scala2CirceCodecRenderer extends CodecRenderer with Render with Logging {

  override def renderCodecsForModels(api: Model.Api, `package`: Option[String], additionalImportPackages: Iterable[String]): ValidatedNec[NotYetImplemented, String] = {
    val renderedCodecs: ValidatedNec[NotYetImplemented, Vector[String]] = api.schemas.collect {
      case (name, obj: Model.Schema.Object) => renderCodecFor(name, obj)
      case (name, stringEnum: Model.Schema.StringEnum) => renderCodecForEnum(name, stringEnum).validNec
      case (name, oneOf: Model.Schema.OneOf) => renderCodecForOneOf(name, oneOf).validNec
    }.map {
      item => item.map(Vector(_))
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

  def renderCodecForEnum(typeName: String, stringEnum: Model.Schema.StringEnum): String = {
    val decoderLines = stringEnum.`enum`.map { line =>
      s"case \"${line}\" => ${typeName}.${line}"
    }
    val encoderLines = stringEnum.`enum`.map { line =>
      s"case ${typeName}.${line} => Json.fromString(\"${line}\")"
    }

    s"""implicit val ${typeName}Codec: Codec[${typeName}] = Codec.from(
       |  (c: HCursor) => {
       |    for {
       |      str <- c.as[String]
       |    } yield {
       |      str match {
       |${indent(8)(decoderLines:_*)}
       |      }
       |    }
       |  },
       |  (a: ${typeName}) => {
       |    a match {
       |${indent(6)(encoderLines:_*)}
       |    }
       |  }
       |)""".stripMargin
  }

  def renderCodecForOneOf(typeName: String, oneOf: Model.Schema.OneOf): String = {
    oneOf.discriminator.fold(
      renderCodecForOneOfWithoutDiscriminator(typeName, oneOf.schemas)
    ) { discriminator =>
      renderCodecForOneOfWithDiscriminator(typeName, discriminator, oneOf.schemas)
    }
  }

  def renderCodecForOneOfWithDiscriminator(typeName: String, discriminator: Discriminator, schemas: NonEmptyList[NamedSchemaRef]): String = {
    val typesSpecifiedInMapping = discriminator.mapping.values.toSet

    val typesNotSpecifiedInMapping = schemas.toList.toSet.diff(typesSpecifiedInMapping)

    val directTypesLines = typesNotSpecifiedInMapping.map { ref =>
      s"case \"${ref.name}\" => c.as[${ref.name}]"
    }.toList

    val mappingDefinitionLines = discriminator.mapping.map { case (name, ref) =>
      s"case \"${name}\" => c.as[${ref.name}]"
    }.toList

    val decoder = s"""Decoder.instance[${typeName}] { c =>
       |  c.downField(${discriminator.propertyName}).as[String] match {
       |${indent(4)(mappingDefinitionLines:_*)}
       |${indent(4)(directTypesLines:_*)}
       |  }
       |}""".stripMargin

    s"""implicit val ${typeName}Codec: Codec[${typeName}] = Codec.from(
       |${indent(2)(decoder)},
       |${indent(2)(renderOneOfEncoder(schemas))}
       |)""".stripMargin
  }

  def renderCodecForOneOfWithoutDiscriminator(typeName: String, schemas: NonEmptyList[NamedSchemaRef]): String = {
    val decoderLines = schemas.map { ref =>
      s"Decoder[${ref.name}].map(identity[${typeName}])"
    }.toList.mkString("," + lineSeparator)

    val encoder = renderOneOfEncoder(schemas)
    val decoder =
      s"""List(
         |${indent(2)(decoderLines)}
         |).reduceLeft(_ or _)
         |""".stripMargin

    s"""implicit val ${typeName}Codec: Codec[${typeName}] = Codec.from(
       |${indent(2)(decoder)},
       |${indent(2)(encoder)}
       |)""".stripMargin
  }

  def renderOneOfEncoder(schemas: NonEmptyList[NamedSchemaRef]): String = {
    val encoderLines = schemas.map { ref =>
      s"case item: ${ref.name} => item.asJson"
    }
    s"""Encoder.instance {
       |${indent(2)(encoderLines.toList:_*)}
       |}""".stripMargin
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
