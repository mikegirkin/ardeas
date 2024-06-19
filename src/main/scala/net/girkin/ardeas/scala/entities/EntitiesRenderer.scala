package net.girkin.ardeas.scala.entities

import cats.data.{NonEmptyChain, given}
import cats.implicits.given
import net.girkin.ardeas.Model.{NamedSchemaRef, SchemaOrRef}
import net.girkin.ardeas.{Logging, Model}
import net.girkin.ardeas.scala.ScalaSpecifics
import net.girkin.ardeas.scala.ScalaSpecifics.TypeNaming.*
import net.girkin.ardeas.RenderUtils.*

object EntitiesRenderer extends Logging {

  def render(api: Model.Api, `package`: Option[String], additionalImports: Vector[String]): String = {
    val packageAndImportsClause = packageAndImportsHeader(`package`, additionalImports)
      .fold("")(
        clause => clause ++ doubleLineSeparator
      )

    val renderedSchemasSection = renderNamedSchemas(api.schemas)
    val renderedRequestBodiesSection = renderNamedRequestBodiesSection(api.namedRequestBodies)
    val renderedResponsesSection = renderNamedResponsesSection(api.namedResponses)

    val content = List(
      renderedSchemasSection,
      renderedRequestBodiesSection,
      renderedResponsesSection
    ).mkString(
      doubleLineSeparator
    )

    s"""${packageAndImportsClause}object Components {
       |${indent(2)(content)}
       |}""".stripMargin
  }

  private def renderNamedSchemas(namedSchemas: Map[String, SchemaOrRef]) = {
    val models = renderModels(namedSchemas)
    s"""object Schemas {
       |${indent(2)(models)}
       |}""".stripMargin
  }

  private def renderNamedRequestBodiesSection(namedRequestBodies: Map[String, Model.RequestBody.Definition]): String = {
    val requestBodies = renderRequestBodies(namedRequestBodies)
    s"""object RequestBodies {
       |${indent(2)(requestBodies: _*)}
       |}""".stripMargin
  }

  private def renderNamedResponsesSection(namedResponses: Map[String, Model.ResponseBody.Definition]): String = {
    val responses = renderResponses(namedResponses)
    s"""object Responses {
       |${indent(2)(responses: _*)}
       |}""".stripMargin
  }

  private def renderModels(namedSchemas: Map[String, SchemaOrRef]) = {
    val adtConformance = namedSchemas.collect {
      case (adtName, Model.Schema.OneOf(refs, discriminator)) =>
        refs.map { ref => ref -> adtName}.toList
    }.flatten.foldLeft(Map.empty[String, NonEmptyChain[String]]) { case (map, (ref, adtName)) =>
      map.updatedWith(typeNameFromReference(ref)) {
        case None => Some(NonEmptyChain.one(adtName))
        case Some(chain) => Some(chain.append(adtName))
      }
    }

    val items = namedSchemas.map { case (name, model) =>
      renderNamedModel(name, model, required = true, adtConformance)
    }
    items.mkString(doubleLineSeparator)
  }

  private def renderRequestBodies(namedRequestBodies: Map[String, Model.RequestBody.Definition]): Vector[String] = {
    namedRequestBodies.collect { case (name, Model.RequestBody.Definition(required, Some(jsonContentSchema))) =>
      renderNamedModel(name, jsonContentSchema, required, adtConformance = Map.empty, useFullyQualifiedRef = true)
    }.toVector
  }

  private def renderResponses(namedResponses: Map[String, Model.ResponseBody.Definition]): Vector[String] = {
    namedResponses.collect { case (name, Model.ResponseBody.Definition(Some(schema))) =>
      renderNamedModel(name, schema, required = true, adtConformance = Map.empty, useFullyQualifiedRef = true)
    }.toVector
  }

  private def renderNamedModel(
    name: String,
    schema: SchemaOrRef,
    required: Boolean,
    adtConformance: Map[String, NonEmptyChain[String]],
    useFullyQualifiedRef: Boolean = false) = {
    schema match {
      case ent: Model.Schema.Object => renderCaseClassForNamedModel(name, ent, adtConformance)
      case arr: Model.Schema.Array => renderNamedArray(name, arr, required, useFullyQualifiedRef)
      case ref: NamedSchemaRef => renderNamedRef(name, ref, required, useFullyQualifiedRef)
      case std: Model.Schema.StandardType => renderNamedStandardType(name, std)
      case stringEnum: Model.Schema.StringEnum => renderNamedStringEnum(name, stringEnum)
      case hmap: Model.Schema.HMap => renderNamedHmap(name, hmap)
      case oneof: Model.Schema.OneOf => renderNamedOneOf(name, oneof)
    }
  }

  private def renderNamedRef(newTypeName: String, ref: NamedSchemaRef, required: Boolean, useFullyQualifiedRef: Boolean = false) = {
    val resolvedRef = typeNameFromReference(ref, useFullyQualifiedRef)
    s"type ${newTypeName} = ${typeForOptional(resolvedRef, required)}"
  }

  private def renderNamedStandardType(newTypeName: String, std: Model.Schema.StandardType) = {
    val scalaType = typeNameFromStandardType(std)
    s"type ${newTypeName} = ${scalaType}"
  }

  private def renderNamedStringEnum(name: String, stringEnum: Model.Schema.StringEnum) = {
    val lineItems = stringEnum.`enum`.map { item =>
      s"case object ${ScalaSpecifics.escapeIfReservedTerm(item)} extends ${name}"
    }
    s"""sealed trait ${name}
       |object ${name} {
       |${indent(2)(lineItems:_*)}
       |}""".stripMargin
  }

  private def renderCaseClassForNamedModel(
    typeName: String,
    schema: Model.Schema.Object,
    adtConformance: Map[String, NonEmptyChain[String]]
  ): String = {
    val fields = schema.fields.map { schemaField =>
      val typeDefinition = ScalaSpecifics.TypeNaming.typeNameForNonAnonymousObjectSchema(schemaField.schema, schemaField.required)
      ScalaSpecifics.Rendering.CaseClassFieldDescription(schemaField.name, typeDefinition)
    }
    val extendClasses = adtConformance.get(typeName).fold(List.empty)(_.toList)
    ScalaSpecifics.Rendering.renderCaseClass(typeName, fields, extendsClasses = extendClasses)
  }

  private def renderNamedArray(name: String, schema: Model.Schema.Array, required: Boolean, useFullyQualified: Boolean): String =
    val arrayTypeQualifier = typeDefinitionForArray(schema, useFullyQualified)
    s"type ${name} = ${typeForOptional(arrayTypeQualifier, required)}"

  private def renderNamedHmap(name: String, schema: Model.Schema.HMap): String = {
    s"type ${name} = ${ScalaSpecifics.TypeNaming.typeDefinitionForHmap(schema)}"
  }

  private def renderNamedOneOf(typeName: String, schema: Model.Schema.OneOf): String = {
    s"sealed trait ${typeName}"
  }
}
