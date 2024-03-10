package net.girkin.ardeas.scala.codecs

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import net.girkin.ardeas.Model
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

    "render a codec for ADT" in {
      val api = Api(
        paths = Vector.empty,
        schemas = Map(
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
          "Transaction" -> Model.Schema.OneOf(
            NonEmptyList.of(NamedSchemaRef("Purchase"), NamedSchemaRef("Refund")),
            None
          )
        ),
        namedRequestBodies = Map.empty,
        namedResponses = Map.empty
      )

      val expected =
        s"""package test_package
           |
           |import io.circe.{HCursor, Codec, Json}
           |import io.circe.syntax.EncoderOps
           |import Components.Schemas._
           |import com.additional
           |
           |object Codecs {
           |  implicit val PurchaseCodec: Codec[Purchase] = Codec.from(
           |    (c: HCursor) => {
           |      for {
           |        id <- c.downField("id").as[Int]
           |        productId <- c.downField("productId").as[Int]
           |        quantity <- c.downField("quantity").as[Int]
           |        totalPaid <- c.downField("totalPaid").as[Int]
           |        timestamp <- c.downField("timestamp").as[java.time.ZonedDateTime]
           |      } yield {
           |        Purchase(id, productId, quantity, totalPaid, timestamp)
           |      }
           |    },
           |    (a: Purchase) => {
           |      Json.obj(
           |        "id" -> a.id.asJson,
           |        "productId" -> a.productId.asJson,
           |        "quantity" -> a.quantity.asJson,
           |        "totalPaid" -> a.totalPaid.asJson,
           |        "timestamp" -> a.timestamp.asJson
           |      )
           |    }
           |  )
           |  implicit val RefundCodec: Codec[Refund] = Codec.from(
           |    (c: HCursor) => {
           |      for {
           |        id <- c.downField("id").as[Int]
           |        purchaseId <- c.downField("purchaseId").as[Int]
           |        refunded <- c.downField("refunded").as[Int]
           |        timestamp <- c.downField("timestamp").as[java.time.ZonedDateTime]
           |      } yield {
           |        Refund(id, purchaseId, refunded, timestamp)
           |      }
           |    },
           |    (a: Refund) => {
           |      Json.obj(
           |        "id" -> a.id.asJson,
           |        "purchaseId" -> a.purchaseId.asJson,
           |        "refunded" -> a.refunded.asJson,
           |        "timestamp" -> a.timestamp.asJson
           |      )
           |    }
           |  )
           |  implicit val TransactionCodec: Codec[Transaction] = Codec.from(
           |    List(
           |      Decoder[Purchase].map(identity[Transaction]),
           |      Decoder[Refund].map(identity[Transaction])
           |    ).reduceLeft(_ or _),
           |    Encoder.instance {
           |      case item: Purchase => item.asJson
           |      case item: Refund => item.asJson
           |    }
           |  )
           |}""".stripMargin

      val rendered = Scala2CirceCodecRenderer.renderCodecsForModels(api, Some("test_package"), Vector("com.additional"))

      rendered shouldBe Valid(expected).toValidatedNec
    }
  }
}
