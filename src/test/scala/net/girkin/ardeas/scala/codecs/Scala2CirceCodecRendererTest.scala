package net.girkin.ardeas.scala.codecs

import cats.data.Validated.Valid
import net.girkin.ardeas.Model.Schema.StandardType
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import net.girkin.ardeas.Model.*

class Scala2CirceCodecRendererTest extends AnyWordSpec with Matchers {

  "Scala2 circe codec" should {
    "render a codec for complete data setup" in {
      val api = Api(
        Vector.empty,
        Map("Pet" -> Schema.makeObject(
          EntityField("id", StandardType("integer"), true),
          EntityField("name", StandardType("string"), true),
          EntityField("tag", StandardType("string"), false)
        )),
        Map(
          "CreatePetRequest" -> RequestBody.Definition(
            true,
            Some(NamedSchemaRef("Pet"))
          )
        ),
        Map("SinglePetResponse" -> ResponseBody.Definition(
          Some(NamedSchemaRef("Pet"))
        ))
      )

      val expected =
        """package test_package
          |
          |import io.circe.{HCursor, Codec, Json}
          |import io.circe.syntax.EncoderOps
          |import Components.Schemas._
          |import com.additional
          |
          |object Codecs {
          |  implicit val PetCodec: Codec[Pet] = Codec.from(
          |    (c: HCursor) => {
          |      for {
          |        id <- c.downField("id").as[Int]
          |        name <- c.downField("name").as[String]
          |        tag <- c.downField("tag").as[Option[String]]
          |      } yield {
          |        Pet(id, name, tag)
          |      }
          |    },
          |    (a: Pet) => {
          |      Json.obj(
          |        "id" -> a.id.asJson,
          |        "name" -> a.name.asJson,
          |        "tag" -> a.tag.asJson
          |      )
          |    }
          |  )
          |}""".stripMargin

      val rendered = Scala2CirceCodecRenderer.renderCodecsForModels(api, Some("test_package"), Vector("com.additional"))

      rendered shouldBe Valid(expected).toValidatedNec
    }
  }
}
