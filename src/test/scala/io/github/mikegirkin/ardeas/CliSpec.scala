package io.github.mikegirkin.ardeas

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Path

class CliSpec extends AnyWordSpec with Matchers {
  "getCommand" should {
    "be able to parse entities render command" in {
      val input = Seq("entities", "./src/test/resources/petstore.yaml", "scala", "./entities.scala", "--package", "org.petstore")

      val expected = RenderEntities(
        Path.of("./src/test/resources/petstore.yaml"),
        "scala",
        Some(Path.of("./entities.scala")),
        Some("org.petstore"),
        Vector.empty
      )

      CLI.getCommand(input) shouldBe Right(expected)
    }

    "be able to parse codecs render command" in {
      val input = Seq("codecs", "./src/test/resources/petstore.yaml", "circe_scala2", "./codecs.scala", "--package", "org.petstore")

      val expected = RenderCodecs(
        Path.of("./src/test/resources/petstore.yaml"),
        "circe_scala2",
        Some(Path.of("./codecs.scala")),
        Some("org.petstore"),
        Vector.empty
      )

      CLI.getCommand(input) shouldBe Right(expected)
    }

    "be able to parse render service command" in {
      val input = Seq("service", "./src/test/resources/petstore_complete.yaml", "http4s_scala2", "./service.scala",
        "--package", "io.github.mikegirkin.petstore.complete.service",
        "--additionalImport", "org.http4s.circe.CirceEntityCodec._",
        "--additionalImport", "io.github.mikegirkin.petstore.complete.service.Codecs._"
      )

      val expected = RenderService(
        Path.of("./src/test/resources/petstore_complete.yaml"),
        "http4s_scala2",
        Some(Path.of("./service.scala")),
        Some("io.github.mikegirkin.petstore.complete.service"),
        Vector(
          "org.http4s.circe.CirceEntityCodec._",
          "io.github.mikegirkin.petstore.complete.service.Codecs._"
        )
      )

      CLI.getCommand(input) shouldBe Right(expected)
    }

    "be able to parse client command" in {
      val input = Seq("client", "./src/test/resources/petstore_complete.yaml", "sttp3_scala2", "./client.scala",
        "--package", "io.github.mikegirkin.petstore.complete.service",
        "--additionalImport", "org.http4s.circe.CirceEntityCodec._",
        "--additionalImport", "io.github.mikegirkin.petstore.complete.service.Codecs._"
      )

      val expected = RenderClient(
        Path.of("./src/test/resources/petstore_complete.yaml"),
        "sttp3_scala2",
        Some(Path.of("./client.scala")),
        Some("io.github.mikegirkin.petstore.complete.service"),
        Vector(
          "org.http4s.circe.CirceEntityCodec._",
          "io.github.mikegirkin.petstore.complete.service.Codecs._"
        )
      )

      CLI.getCommand(input) shouldBe Right(expected)
    }
  }
}
