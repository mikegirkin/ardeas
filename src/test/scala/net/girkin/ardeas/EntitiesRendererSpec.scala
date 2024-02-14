package net.girkin.ardeas

import net.girkin.ardeas.Model.*
import net.girkin.ardeas.scala.entities.EntitiesRenderer
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
        )
      )
      val api = Api(
        Vector.empty,
        schemas = Map(
          "Pet" -> petEntity,
          "Pets" -> Schema.Array(NamedSchemaRef("Pet")),
          "RenamedPet" -> NamedSchemaRef("Pet"),
          "UserId" -> Schema.StandardType("integer", Some("int64"))
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
          |      tags: Vector[String]
          |    )
          |    type Pets = Vector[Pet]
          |    type RenamedPet = Pet
          |    type UserId = Long
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
