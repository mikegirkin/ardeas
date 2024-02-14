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
          |            tag.convertTo[String]
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
          |            birthDate.convertTo[java.time.LocalDate],
          |            created.convertTo[java.time.ZonedDateTime],
          |            weight.convertTo[Double],
          |            height.convertTo[Float],
          |            tag.convertTo[String]
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
  }
}