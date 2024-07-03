package io.github.mikegirkin.ardeas

import java.nio.file.Path
import com.monovore.decline.*
import cats.implicits.*

enum CliParsingError(message: String) {
  case GenericError(message: String) extends CliParsingError(message)
  case UnknownCommand extends CliParsingError("Passed uknown command")
}

object CLI {
  private object CommandDefinitions {
    val inputYaml = Opts.argument[String](metavar = "inputYaml")
    val generator = Opts.argument[String](metavar = "generator")
    val outputFile = Opts.argument[String](metavar = "outputFile").orNone
    val additionalImport = Opts.options[String]("additionalImport", help = "Specifies additional imports to add to scala file").orNone
    val `package` = Opts.option[String]("package", help = "Specifies the package name to write in the scala file").orNone

    val renderEntities = Opts.subcommand("entities", "Render entities")((inputYaml, generator, outputFile, additionalImport, `package`).tupled)
      .map { case (inputYaml, generator, outputFile, additionalImports, `package`@_) =>
        RenderEntities(
          Path.of(inputYaml),
          generator,
          outputFile.map(x => Path.of(x)),
          `package`,
          additionalImports.fold(Vector.empty)(_.toIterable.toVector)
        )
      }

    val renderCodecs = Opts.subcommand("codecs", "Render codecs")((inputYaml, generator, outputFile, additionalImport, `package`).tupled)
      .map { case (inputYaml, generator, outputFile, additionalImports, `package`@_) =>
        RenderCodecs(
          Path.of(inputYaml),
          generator,
          outputFile.map(x => Path.of(x)),
          `package`,
          additionalImports.fold(Vector.empty)(_.toIterable.toVector)
        )
      }

    val renderService = Opts.subcommand("service", "Render service")((inputYaml, generator, outputFile, additionalImport, `package`).tupled)
      .map { case (inputYaml, generator, outputFile, additionalImports, `package`@_) =>
        RenderService(
          Path.of(inputYaml),
          generator,
          outputFile.map(x => Path.of(x)),
          `package`,
          additionalImports.fold(Vector.empty)(_.toIterable.toVector)
        )
      }

    val renderClient = Opts.subcommand("client", "Render client")((inputYaml, generator, outputFile, additionalImport, `package`).tupled)
      .map { case (inputYaml, generator, outputFile, additionalImports, `package`@_) =>
        RenderClient(
          Path.of(inputYaml),
          generator,
          outputFile.map(x => Path.of(x)),
          `package`,
          additionalImports.fold(Vector.empty)(_.toIterable.toVector)
        )
      }

    val command = Command("ardeas", "Generating code from openapi specifications")(
      renderEntities orElse renderCodecs orElse renderService orElse renderClient
    )
  }

  def getCommand(cliArgs: Seq[String]): Either[CliParsingError, CliCommand] = {
    CommandDefinitions.command.parse(cliArgs).leftMap(
      { help =>
        println(help)
        println(help.errors)
        CliParsingError.UnknownCommand
      }
    )
  }
}
