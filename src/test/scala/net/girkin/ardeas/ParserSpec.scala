package net.girkin.ardeas

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import net.girkin.ardeas.Model.Parameter.{PathParameter, QueryParameter}
import net.girkin.ardeas.Model.{Discriminator, NamedSchemaRef, Parameter, RequestBody, ResponseBody}
import net.girkin.ardeas.parser.Parser
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserSpec extends AnyWordSpec with Matchers with Inside:

  "parser" should {
    "be able to parse petstore" in {
      val url = getClass.getClassLoader.getResource("petstore.yaml").toURI

      val result = Parser.parse(url)

      val expected = Model.Api(
        Vector(
          Model.HttpOperation(Model.Path.of("pets"), Model.HttpVerb.Get, Some("listPets"),
            Vector(
              QueryParameter("limit",Model.Schema.StandardType("integer", Some("int32")), false)
            ),
            Vector(
              Model.Response(200, ResponseBody.Definition(Some(Model.NamedSchemaRef("Pets")))),
              Model.Response(Model.Default, ResponseBody.Definition(Some(Model.NamedSchemaRef("Error"))))
            ),
            None
          ),
          Model.HttpOperation(Model.Path.of("pets"), Model.HttpVerb.Post, Some("createPets"),
            Vector.empty,
            Vector(
              Model.Response(201, ResponseBody.Definition(None)),
              Model.Response(Model.Default, ResponseBody.Definition(Some(Model.NamedSchemaRef("Error"))))
            ),
            Some(Model.RequestBody.NamedRef("CreatePetBody"))
          ),
          Model.HttpOperation(Model.Path(List(Model.PathSegment.StringSegment("pets"), Model.PathSegment.TemplatedParameter("petId"))), Model.HttpVerb.Get, Some("showPetById"),
            Vector(
              PathParameter("petId", Model.Schema.StandardType("string", None))
            ),
            Vector(
              Model.Response(200, ResponseBody.Definition(Some(Model.NamedSchemaRef("Pet")))),
              Model.Response(Model.Default, ResponseBody.Definition(Some(Model.NamedSchemaRef("Error"))))
            ),
            None
          ),
          Model.HttpOperation(Model.Path(List(Model.PathSegment.StringSegment("healthcheck"))), Model.HttpVerb.Get, Some("getHealthCheck"),
            Vector.empty,
            Vector(
              Model.Response(200, ResponseBody.Definition(Some(Model.Schema.HMap(Model.Schema.StandardType("integer", Some("int32"))))))
            ),
            None
          )
        ),
        Map(
          "Pet" -> Model.Schema.makeObject(
            Model.EntityField("id", Model.Schema.StandardType("integer", Some("int64")), true),
            Model.EntityField("name", Model.Schema.StandardType("string", None), true),
            Model.EntityField("tag", Model.Schema.StandardType("string", None), false),
          ),
          "Pets" -> Model.Schema.Array(Model.NamedSchemaRef("Pet")),
          "Error" -> Model.Schema.makeObject(
            Model.EntityField("code", Model.Schema.StandardType("integer", Some("int32")), true),
            Model.EntityField("message", Model.Schema.StandardType("string", None), true)
          )
        ),
        Map(
          "CreatePetBody" -> Model.RequestBody.Definition(
            true,
            Some(Model.NamedSchemaRef("Pet"))
          )
        ),
        Map.empty
      )

      result shouldBe Valid(expected)
    }

    "be able to parse petstore_ext" in {
      val url = getClass.getClassLoader.getResource("petstore_ext.yaml").toURI

      val expected = Model.Api(
        Vector(
          Model.HttpOperation(Model.Path.of("pets"), Model.HttpVerb.Get, Some("listPets"),
            Vector(
              QueryParameter("limit", Model.Schema.StandardType("integer", Some("int32")), false)
            ),
            Vector(
              Model.Response(200, ResponseBody.Definition(Some(Model.NamedSchemaRef("Pets")))),
              Model.Response(201, ResponseBody.Definition(Some(Model.Schema.StandardType("integer", Some("int64"))))),
              Model.Response(Model.Default, ResponseBody.Definition(Some(Model.NamedSchemaRef("Error")))),
            ),
            None
          ),
          Model.HttpOperation(Model.Path.of("pets"), Model.HttpVerb.Post, Some("createPets"),
            Vector.empty,
            Vector(
              Model.Response(201, ResponseBody.Definition(None)),
              Model.Response(Model.Default, ResponseBody.Definition(Some(Model.NamedSchemaRef("Error"))))
            ),
            Some(RequestBody.NamedRef("CreatePetRequest"))
          ),
          Model.HttpOperation(Model.Path(List(Model.PathSegment.StringSegment("pets"), Model.PathSegment.TemplatedParameter("petId"))), Model.HttpVerb.Get, Some("showPetById"),
            Vector(
              PathParameter("petId", Model.Schema.StandardType("string", None))
            ),
            Vector(
              Model.Response(200, Model.ResponseBody.NamedRef("SinglePetResponse")),
              Model.Response(Model.Default, ResponseBody.Definition(Some(Model.NamedSchemaRef("Error"))))
            ),
            None
          ),
          Model.HttpOperation(
            Model.Path(List(Model.PathSegment.StringSegment("pets"), Model.PathSegment.TemplatedParameter("petId"))),
            Model.HttpVerb.Put,
            Some("updatePet"),
            Vector(
              PathParameter("petId", Model.Schema.StandardType("string", None))
            ),
            Vector(
              Model.Response(200, Model.ResponseBody.NamedRef("SinglePetResponse")),
              Model.Response(Model.Default, ResponseBody.Definition(Some(Model.NamedSchemaRef("Error"))))
            ),
            Some(RequestBody.NamedRef("UpdatePetRequest"))
          ),
          Model.HttpOperation(
            Model.Path(List(Model.PathSegment.StringSegment("transactions"), Model.PathSegment.TemplatedParameter("userId"))),
            Model.HttpVerb.Get,
            Some("getTransactionById"),
            Vector(
              PathParameter("userId", Model.Schema.StandardType("string", None))
            ),
            Vector(
              Model.Response(200, ResponseBody.Definition(Some(Model.NamedSchemaRef("Transaction"))))
            ),
            None
          )
        ),
        Map(
          "Species" -> Model.Schema.StringEnum(List("cat", "dog", "hamster")),
          "Pet" -> Model.Schema.makeObject(
            Model.EntityField("id", Model.Schema.StandardType("integer", Some("int64")), true),
            Model.EntityField("name", Model.Schema.StandardType("string", None), true),
            Model.EntityField("birthDate", Model.Schema.StandardType("string", Some("date")), false),
            Model.EntityField("created", Model.Schema.StandardType("string", Some("datetime")), false),
            Model.EntityField("weight", Model.Schema.StandardType("number", None), false),
            Model.EntityField("height", Model.Schema.StandardType("number", Some("float")), false),
            Model.EntityField("tag", Model.Schema.StandardType("string", None), false),
            Model.EntityField("species", NamedSchemaRef("Species"), true)
          ),
          "Pets" -> Model.Schema.Array(Model.NamedSchemaRef("Pet")),
          "Messages" -> Model.Schema.Array(Model.Schema.StandardType("string")),
          "Error" -> Model.Schema.OneOf(
            NonEmptyList.of(
              NamedSchemaRef("NotFound"),
              NamedSchemaRef("NoAccess"),
              NamedSchemaRef("UnsupportedOperation")
            ),
            Some(
              Discriminator(
                "type",
                Map(
                  "unsupported" -> NamedSchemaRef("UnsupportedOperation")
                )
              )
            )
          ),
          "NotFound" -> Model.Schema.makeObject(
            Model.EntityField("type", Model.Schema.StandardType("string", None), true),
            Model.EntityField("message", Model.Schema.StandardType("string", None), true),
          ),
          "NoAccess" -> Model.Schema.makeObject(
            Model.EntityField("type", Model.Schema.StandardType("string", None), true),
            Model.EntityField("message", Model.Schema.StandardType("string", None), true),
          ),
          "UnsupportedOperation" -> Model.Schema.makeObject(
            Model.EntityField("type", Model.Schema.StandardType("string", None), true),
            Model.EntityField("message", Model.Schema.StandardType("string", None), true),
          ),
          "Transaction" -> Model.Schema.OneOf(
            NonEmptyList.of(NamedSchemaRef("Purchase"), NamedSchemaRef("Refund")),
            None
          ),
          "Purchase" -> Model.Schema.makeObject(
            Model.EntityField("id", Model.Schema.StandardType("integer", None), true),
            Model.EntityField("productId", Model.Schema.StandardType("integer", None), true),
            Model.EntityField("quantity", Model.Schema.StandardType("integer", None), true),
            Model.EntityField("totalPaid", Model.Schema.StandardType("integer", None), true),
            Model.EntityField("timestamp", Model.Schema.StandardType("string", Some("datetime")), true)
          ),
          "Refund" -> Model.Schema.makeObject(
            Model.EntityField("id", Model.Schema.StandardType("integer", None), true),
            Model.EntityField("purchaseId", Model.Schema.StandardType("integer", None), true),
            Model.EntityField("refunded", Model.Schema.StandardType("integer", None), true),
            Model.EntityField("timestamp", Model.Schema.StandardType("string", Some("datetime")), true)
          ),
        ),
        Map(
          "CreatePetRequest" -> Model.RequestBody.Definition(
            true,
            Some(Model.NamedSchemaRef("Pet"))
          ),
          "UpdatePetRequest" -> Model.RequestBody.Definition(
            false,
            Some(Model.NamedSchemaRef("Pet"))
          )
        ),
        Map(
          "SinglePetResponse" -> Model.ResponseBody.Definition(
            Some(Model.NamedSchemaRef("Pet"))
          )
        )
      )

      val result = Parser.parse(url)

      inside(result) {
        case Valid(parsedApi) => {
          parsedApi.schemas should contain theSameElementsAs expected.schemas
          parsedApi.paths shouldBe expected.paths
          parsedApi.namedRequestBodies shouldBe expected.namedRequestBodies
          parsedApi.namedResponses shouldBe expected.namedResponses

          parsedApi shouldBe expected
        }
      }
    }
  }