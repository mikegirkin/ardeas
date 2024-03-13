package net.girkin.ardeas.scala.codecs

import net.girkin.ardeas.Model
import net.girkin.ardeas.Model.EntityField
import net.girkin.ardeas.Model.Schema.StandardType
import net.girkin.ardeas.Model.Schema
import net.girkin.ardeas.parser.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SprayCodecRendererSpec extends AnyWordSpec with Matchers {
  "CodecsRenderer" should {
    "be able to render a model" in {
      val schema = Schema.makeObject(
        EntityField("id", StandardType("integer"), true)
      )

      val rendered: String = SprayCodecRenderer.renderCodecFor("Pet", schema)
      val expected: String =
        """implicit val PetFormat: RootJsonFormat[Pet] = new RootJsonFormat[Pet] {
          |  override def write(obj: Pet): JsValue = JsObject(
          |    "id" -> JsNumber(obj.id)
          |  )
          |  override def read(json: JsValue): Pet = {
          |    json.asJsObject.getFields("id") match {
          |      case Seq(id) =>
          |        Pet(
          |          id.convertTo[Int]
          |        )
          |      case _ => throw DeserializationException(s"Could not deserialize ${json} to Pet")
          |    }
          |  }
          |}""".stripMargin

      rendered shouldBe expected
    }

    "be able to render petStore" in {
      val url = getClass.getClassLoader.getResource("petstore.yaml").toURI

      val resultV = Parser.parse(url)
      val result = resultV.toEither

      result should matchPattern {
        case Right(_) =>
      }

      val rendered = SprayCodecRenderer.renderCodecsForModels(result.toOption.get, Some("my_package"), Vector("com.additional"))
        .toOption.get

      val expected =
        """package my_package
          |
          |import Components.Schemas._
          |import spray.json._
          |import spray.json.DefaultJsonProtocol._
          |import com.additional
          |
          |object Codecs {
          |  implicit val PetFormat: RootJsonFormat[Pet] = new RootJsonFormat[Pet] {
          |    override def write(obj: Pet): JsValue = JsObject(
          |      "id" -> JsNumber(obj.id),
          |      "name" -> JsString(obj.name.toString),
          |      "tag" -> JsString(obj.tag.toString)
          |    )
          |    override def read(json: JsValue): Pet = {
          |      json.asJsObject.getFields("id", "name", "tag") match {
          |        case Seq(id, name, tag) =>
          |          Pet(
          |            id.convertTo[Long],
          |            name.convertTo[String],
          |            tag.convertTo[Option[String]]
          |          )
          |        case _ => throw DeserializationException(s"Could not deserialize ${json} to Pet")
          |      }
          |    }
          |  }
          |  implicit val ErrorFormat: RootJsonFormat[Error] = new RootJsonFormat[Error] {
          |    override def write(obj: Error): JsValue = JsObject(
          |      "code" -> JsNumber(obj.code),
          |      "message" -> JsString(obj.message.toString)
          |    )
          |    override def read(json: JsValue): Error = {
          |      json.asJsObject.getFields("code", "message") match {
          |        case Seq(code, message) =>
          |          Error(
          |            code.convertTo[Int],
          |            message.convertTo[String]
          |          )
          |        case _ => throw DeserializationException(s"Could not deserialize ${json} to Error")
          |      }
          |    }
          |  }
          |}""".stripMargin

      rendered shouldBe expected
    }

    "be able to render petStore_ext" in {
      val url = getClass.getClassLoader.getResource("petstore_ext.yaml").toURI

      val resultV = Parser.parse(url)
      val result = resultV.toEither

      result should matchPattern {
        case Right(_) =>
      }

      val rendered = SprayCodecRenderer.renderCodecsForModels(result.toOption.get, Some("my_package"), Vector("com.additional"))
        .toOption.get

      val expected =
        """package my_package
          |
          |import Components.Schemas._
          |import spray.json._
          |import spray.json.DefaultJsonProtocol._
          |import com.additional
          |
          |object Codecs {
          |  implicit val PetFormat: RootJsonFormat[Pet] = new RootJsonFormat[Pet] {
          |    override def write(obj: Pet): JsValue = JsObject(
          |      "id" -> JsNumber(obj.id),
          |      "name" -> JsString(obj.name.toString),
          |      "birthDate" -> JsString(obj.birthDate.toString),
          |      "created" -> JsString(obj.created.toString),
          |      "weight" -> JsNumber(obj.weight),
          |      "height" -> JsNumber(obj.height),
          |      "tag" -> JsString(obj.tag.toString)
          |    )
          |    override def read(json: JsValue): Pet = {
          |      json.asJsObject.getFields("id", "name", "birthDate", "created", "weight", "height", "tag") match {
          |        case Seq(id, name, birthDate, created, weight, height, tag) =>
          |          Pet(
          |            id.convertTo[Long],
          |            name.convertTo[String],
          |            birthDate.convertTo[Option[java.time.LocalDate]],
          |            created.convertTo[Option[java.time.ZonedDateTime]],
          |            weight.convertTo[Option[Double]],
          |            height.convertTo[Option[Float]],
          |            tag.convertTo[Option[String]]
          |          )
          |        case _ => throw DeserializationException(s"Could not deserialize ${json} to Pet")
          |      }
          |    }
          |  }
          |  implicit val NotFoundFormat: RootJsonFormat[NotFound] = new RootJsonFormat[NotFound] {
          |    override def write(obj: NotFound): JsValue = JsObject(
          |      "type" -> JsString(obj.type.toString),
          |      "message" -> JsString(obj.message.toString)
          |    )
          |    override def read(json: JsValue): NotFound = {
          |      json.asJsObject.getFields("type", "message") match {
          |        case Seq(type, message) =>
          |          NotFound(
          |            type.convertTo[String],
          |            message.convertTo[String]
          |          )
          |        case _ => throw DeserializationException(s"Could not deserialize ${json} to NotFound")
          |      }
          |    }
          |  }
          |  implicit val NoAccessFormat: RootJsonFormat[NoAccess] = new RootJsonFormat[NoAccess] {
          |    override def write(obj: NoAccess): JsValue = JsObject(
          |      "type" -> JsString(obj.type.toString),
          |      "message" -> JsString(obj.message.toString)
          |    )
          |    override def read(json: JsValue): NoAccess = {
          |      json.asJsObject.getFields("type", "message") match {
          |        case Seq(type, message) =>
          |          NoAccess(
          |            type.convertTo[String],
          |            message.convertTo[String]
          |          )
          |        case _ => throw DeserializationException(s"Could not deserialize ${json} to NoAccess")
          |      }
          |    }
          |  }
          |  implicit val UnsupportedOperationFormat: RootJsonFormat[UnsupportedOperation] = new RootJsonFormat[UnsupportedOperation] {
          |    override def write(obj: UnsupportedOperation): JsValue = JsObject(
          |      "type" -> JsString(obj.type.toString),
          |      "message" -> JsString(obj.message.toString)
          |    )
          |    override def read(json: JsValue): UnsupportedOperation = {
          |      json.asJsObject.getFields("type", "message") match {
          |        case Seq(type, message) =>
          |          UnsupportedOperation(
          |            type.convertTo[String],
          |            message.convertTo[String]
          |          )
          |        case _ => throw DeserializationException(s"Could not deserialize ${json} to UnsupportedOperation")
          |      }
          |    }
          |  }
          |  implicit val PurchaseFormat: RootJsonFormat[Purchase] = new RootJsonFormat[Purchase] {
          |    override def write(obj: Purchase): JsValue = JsObject(
          |      "id" -> JsNumber(obj.id),
          |      "productId" -> JsNumber(obj.productId),
          |      "quantity" -> JsNumber(obj.quantity),
          |      "totalPaid" -> JsNumber(obj.totalPaid),
          |      "timestamp" -> JsString(obj.timestamp.toString)
          |    )
          |    override def read(json: JsValue): Purchase = {
          |      json.asJsObject.getFields("id", "productId", "quantity", "totalPaid", "timestamp") match {
          |        case Seq(id, productId, quantity, totalPaid, timestamp) =>
          |          Purchase(
          |            id.convertTo[Int],
          |            productId.convertTo[Int],
          |            quantity.convertTo[Int],
          |            totalPaid.convertTo[Int],
          |            timestamp.convertTo[java.time.ZonedDateTime]
          |          )
          |        case _ => throw DeserializationException(s"Could not deserialize ${json} to Purchase")
          |      }
          |    }
          |  }
          |  implicit val RefundFormat: RootJsonFormat[Refund] = new RootJsonFormat[Refund] {
          |    override def write(obj: Refund): JsValue = JsObject(
          |      "id" -> JsNumber(obj.id),
          |      "purchaseId" -> JsNumber(obj.purchaseId),
          |      "refunded" -> JsNumber(obj.refunded),
          |      "timestamp" -> JsString(obj.timestamp.toString)
          |    )
          |    override def read(json: JsValue): Refund = {
          |      json.asJsObject.getFields("id", "purchaseId", "refunded", "timestamp") match {
          |        case Seq(id, purchaseId, refunded, timestamp) =>
          |          Refund(
          |            id.convertTo[Int],
          |            purchaseId.convertTo[Int],
          |            refunded.convertTo[Int],
          |            timestamp.convertTo[java.time.ZonedDateTime]
          |          )
          |        case _ => throw DeserializationException(s"Could not deserialize ${json} to Refund")
          |      }
          |    }
          |  }
          |}""".stripMargin

      rendered shouldBe expected
    }
  }
}
