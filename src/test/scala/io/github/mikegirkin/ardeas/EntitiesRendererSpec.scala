package io.github.mikegirkin.ardeas

import _root_.scala.collection.immutable.ListMap
import cats.data.NonEmptyList
import io.github.mikegirkin.ardeas.Model.*
import io.github.mikegirkin.ardeas.scala.entities.EntitiesRenderer
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EntitiesRendererSpec extends AnyWordSpec with Matchers:

  "Entities renderer" should {
    "be able to render case classes" in {
      val petEntity = Schema.makeObject(
        EntityField("id", Schema.StandardType("integer", None), true),
        EntityField("name",Schema.StandardType("string", None), true),
        EntityField("birthDate", Schema.StandardType("string", Some("date")), false),
        EntityField("created", Schema.StandardType("string", Some("datetime")), false),
        EntityField("weight", Schema.StandardType("number", None), false),
        EntityField("height", Schema.StandardType("number", Some("float")), false),
        EntityField(
          "tags",
          Schema.Array(
            Schema.StandardType("string", None)
          ), true
        ),
        EntityField(
          "species", NamedSchemaRef("Species"), true
        )
      )
      val purchase = Schema.makeObject(
        EntityField("id", Schema.StandardType("integer", None), true),
        EntityField("totalPaid", Schema.StandardType("integer", None), true)
      )
      val refund = Schema.makeObject(
        EntityField("id", Schema.StandardType("integer", None), true),
        EntityField("purchaseId", Schema.StandardType("integer", None), true),
        EntityField("totalRefunded", Schema.StandardType("integer", None), true),
      )
      val transaction = Schema.OneOf(
        NonEmptyList.of(
          NamedSchemaRef("Purchase"),
          NamedSchemaRef("Refund")
        ),
        discriminator = None
      )
      val api = Api(
        Vector.empty,
        schemas = ListMap(
          "Pet" -> petEntity,
          "Pets" -> Schema.Array(NamedSchemaRef("Pet")),
          "RenamedPet" -> NamedSchemaRef("Pet"),
          "UserId" -> Schema.StandardType("integer", Some("int64")),
          "Transaction" -> transaction,
          "Purchase" -> purchase,
          "Refund" -> refund,
          "Species" -> Schema.StringEnum(List("cat", "dog", "hamster"))
        ),
        namedRequestBodies = Map(
          "CreatePetBody" -> RequestBody.Definition(
            true,
            Some(NamedSchemaRef("Pet"))
          ),
          "UpdatePetBody" -> RequestBody.Definition(
            false,
            Some(NamedSchemaRef("Pet"))
          )
        ),
        namedResponses = Map(
          "SinglePetResponse" -> ResponseBody.Definition(
            Some(NamedSchemaRef("Pet"))
          )
        )
      )

      val rendered = EntitiesRenderer.render(api, None, Vector.empty)
      val expected =
        """object Components {
          |  object Schemas {
          |    case class Pet(
          |      id: Int,
          |      name: String,
          |      birthDate: Option[java.time.LocalDate],
          |      created: Option[java.time.ZonedDateTime],
          |      weight: Option[Double],
          |      height: Option[Float],
          |      tags: Vector[String],
          |      species: Species
          |    )
          |
          |    type Pets = Vector[Pet]
          |
          |    type RenamedPet = Pet
          |
          |    type UserId = Long
          |
          |    sealed trait Transaction
          |
          |    case class Purchase(
          |      id: Int,
          |      totalPaid: Int
          |    ) extends Transaction
          |
          |    case class Refund(
          |      id: Int,
          |      purchaseId: Int,
          |      totalRefunded: Int
          |    ) extends Transaction
          |
          |    sealed trait Species
          |    object Species {
          |      case object cat extends Species
          |      case object dog extends Species
          |      case object hamster extends Species
          |    }
          |  }
          |
          |  object RequestBodies {
          |    type CreatePetBody = Components.Schemas.Pet
          |    type UpdatePetBody = Option[Components.Schemas.Pet]
          |  }
          |
          |  object Responses {
          |    type SinglePetResponse = Components.Schemas.Pet
          |  }
          |}""".stripMargin

      rendered shouldBe expected
    }
  }
