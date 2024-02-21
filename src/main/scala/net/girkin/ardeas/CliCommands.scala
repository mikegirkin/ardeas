package net.girkin.ardeas

import cats.syntax.either.*
import net.girkin.ardeas
import net.girkin.ardeas.parser.Parser
import net.girkin.ardeas.scala.codecs.{CodecRenderer, Scala2CirceCodecRenderer, SprayCodecRenderer}
import net.girkin.ardeas.scala.entities.EntitiesRenderer
import net.girkin.ardeas.scala.http4s.{ClientRenderer, ServiceRenderer}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

object CliHelpers {
  def writeTextToFile(filePath: Path)(text: String) = {
    val absolutePath = filePath.toAbsolutePath
    val directory = filePath.getParent
    Files.createDirectories(directory)
    Files.write(absolutePath, text.getBytes(StandardCharsets.UTF_8))
  }

  def writeRenderResult(renderToFilePath: Option[Path])(renderedText: String) = {
    val outAction = renderToFilePath.fold {
      (text: String) => println(text)
    } {
      renderToFilePath => CliHelpers.writeTextToFile(renderToFilePath)
    }
    outAction(renderedText)
  }
}

sealed trait CliCommand {
  def run(): Unit
}

final case class RenderEntities(openapiFilePath: Path, renderer: String, renderToFilePath: Option[Path], `package`: Option[String], additionalImports: Vector[String]) extends CliCommand {
  def run(): Unit = {
    val apiModelV = Parser.parse(openapiFilePath.toUri)
    val textE = for {
      apiModel <- apiModelV.toEither.leftMap { errors =>
        s"Failed to parse OpenAPI spec. Errors: ${errors.iterator.mkString(System.lineSeparator())}"
      }
      text = EntitiesRenderer.render(apiModel, `package`, additionalImports)
    } yield {
      text
    }
    textE.fold(
      { error =>
        println(error)
      },
      {
        CliHelpers.writeRenderResult(renderToFilePath)
      }
    )
  }
}

final case class RenderCodecs(openapiFilePath: Path, renderer: String, renderToFilePath: Option[Path], `package`: Option[String], additionalImports: Vector[String]) extends CliCommand {
  private val codecImplementations: Map[String, CodecRenderer] = Map(
    "spray" -> SprayCodecRenderer,
    "circe_scala2" -> Scala2CirceCodecRenderer
  )

  override def run(): Unit = {
    val result = for {
      renderer <- codecImplementations.get(renderer)
        .toRight(s"Unknown renderer implementation. Known implementations: ${codecImplementations.keys.mkString(",")}")
      apiModelV = Parser.parse(openapiFilePath.toUri)
      apiModel <- apiModelV.toEither.leftMap { errors =>
        s"Failed to parse OpenAPI spec. Errors: ${errors.iterator.mkString(System.lineSeparator())}"
      }
      text <- renderer.renderCodecsForModels(apiModel, `package`, additionalImports).toEither.leftMap { errors =>
        s"Failed to render codecs. Errors: ${errors.iterator.mkString(System.lineSeparator())}"
      }
    } yield {
      text
    }
    result.fold(
      { error =>
        println(error)
      },
      {
        CliHelpers.writeRenderResult(renderToFilePath)
      }
    )
  }
}

final case class RenderService(openapiFilePath: Path, renderer: String, renderToFilePath: Option[Path], `package`: Option[String], additionalImports: Vector[String]) extends CliCommand {
  override def run(): Unit = {
    val apiModelV = Parser.parse(openapiFilePath.toUri)
    val renderer = ServiceRenderer
    val result = for {
      apiModel <- apiModelV.toEither.leftMap { errors =>
        s"Failed to parse OpenAPI spec. Errors: ${errors.iterator.mkString(System.lineSeparator())}"
      }
      text = renderer.renderService(apiModel, `package`, additionalImports)
    } yield {
      text
    }
    result.fold({ error =>
      println(error)
    }, {
      CliHelpers.writeRenderResult(renderToFilePath)
    })
  }
}

final case class RenderClient(openapiFilePath: Path, renderer: String, renderToFilePath: Option[Path], `package`: Option[String], additionalImports: Vector[String]) extends CliCommand {
  override def run(): Unit = {
    val apiModelV = Parser.parse(openapiFilePath.toUri)
    val renderer = ClientRenderer
    val result = for {
      apiModel <- apiModelV.toEither.leftMap { errors =>
        s"Failed to parse OpenAPI spec. Errors: ${errors.iterator.mkString(System.lineSeparator(), System.lineSeparator(), "")}"
      }
      text = renderer.renderClient(apiModel, `package`, additionalImports)
    } yield {
      text
    }
    result.fold({ error =>
      println(error)
    }, {
      CliHelpers.writeRenderResult(renderToFilePath)
    })
  }
}
