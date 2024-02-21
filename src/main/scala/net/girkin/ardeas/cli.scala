package net.girkin.ardeas

import net.girkin.ardeas.scala.codecs.{Scala2CirceCodecRenderer, SprayCodecRenderer}
import net.girkin.ardeas.scala.entities.EntitiesRenderer

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import _root_.scala.util.CommandLineParser as CLP
import net.girkin.ardeas.CliCommand

enum CliParsingError(message: String) {
  case GenericError(message: String) extends CliParsingError(message)
  case UnknownCommand extends CliParsingError("Passed uknown command")
}

object CLI {
  private val commandParsers = Map(
    "entities" -> entitiesCommand,
    "codecs" -> codecsCommand,
    "service" -> serviceCommand,
    "client" -> clientCommand
  )

  def getCommand(cliArgs: Array[String]): Either[CliParsingError, CliCommand] = {
    val command = CLP.parseArgument[String](cliArgs, 0)
    val namedArgs: Map[String, Vector[String]] = parseNamedArguments(cliArgs)
    for {
      createCmd <- commandParsers.get(command).toRight(CliParsingError.UnknownCommand)
      cmd = createCmd(cliArgs.drop(1), namedArgs)
    } yield {
      cmd
    }
  }

  private def parseNamedArguments(args: Array[String]): Map[String, Vector[String]] = {
    val namedArgsIndices = args.zipWithIndex.collect {
      case (arg, index) if arg.startsWith("--") => index
    }

    val argNameValuePairs: Array[(String, Option[String])] =
      namedArgsIndices.map { index =>
        val argName = args.lift(index).map(_.drop(2))
        val argValue = args.lift(index + 1)
        argName.map(name => name -> argValue)
      }.collect {
        case Some(x) => x
      }

    argNameValuePairs.foldLeft(Map.empty[String, Vector[String]]) { case (acc, (argName, argValueOpt)) =>
      acc.updatedWith(argName) { existingValuesListOpt =>
        existingValuesListOpt.fold(
          argValueOpt.map(x => Vector(x))
        ) { valueList =>
          Some(valueList ++ argValueOpt.toVector)
        }
      }
    }
  }

  private def entitiesCommand(commandArgs: Array[String], namedArgs: Map[String, Vector[String]]): RenderEntities = {
    val openapiFilePath = CLP.parseArgument[String](commandArgs, 0)
    val renderer = CLP.parseArgument[String](commandArgs, 1)
    val renderToFilePath = if (commandArgs.length >= 3) Some(CLP.parseArgument[String](commandArgs, 2)) else None
    val openapiFile = Path.of(openapiFilePath).toAbsolutePath
    val renderToFile = renderToFilePath.map { str => Path.of(str).toAbsolutePath }
    val packageOpt = namedArgs.get("package").flatMap(_.headOption)
    val additionalImports = namedArgs.getOrElse("additionalImport", Vector.empty)
    RenderEntities(openapiFile, renderer, renderToFile, packageOpt, additionalImports)
  }

  private def codecsCommand(commandArgs: Array[String], namedArgs: Map[String, Vector[String]]): RenderCodecs = {
    val openapiFilePath = CLP.parseArgument[String](commandArgs, 0)
    val renderer = CLP.parseArgument[String](commandArgs, 1)
    val renderToFilePath = if (commandArgs.length >= 3) Some(CLP.parseArgument[String](commandArgs, 2)) else None
    val openapiFile = Path.of(openapiFilePath).toAbsolutePath
    val renderToFile = renderToFilePath.map { str => Path.of(str).toAbsolutePath }
    val packageOpt = namedArgs.get("package").flatMap(_.headOption)
    val additionalImports = namedArgs.getOrElse("additionalImport", Vector.empty)
    RenderCodecs(openapiFile, renderer, renderToFile, packageOpt, additionalImports)
  }

  private def serviceCommand(commandArgs: Array[String], namedArgs: Map[String, Vector[String]]): RenderService = {
    val openapiFilePath = CLP.parseArgument[String](commandArgs, 0)
    val renderer = CLP.parseArgument[String](commandArgs, 1)
    val renderToFilePath = if(commandArgs.length >= 3) Some(CLP.parseArgument[String](commandArgs, 2)) else None
    val openapiFile = Path.of(openapiFilePath).toAbsolutePath
    val renderToFile = renderToFilePath.map { str => Path.of(str).toAbsolutePath }
    val packageOpt = namedArgs.get("package").flatMap(_.headOption)
    val additionalImports = namedArgs.getOrElse("additionalImport", Vector.empty)
    RenderService(openapiFile, renderer, renderToFile, packageOpt, additionalImports)
  }

  private def clientCommand(commandArgs: Array[String], namedArgs: Map[String, Vector[String]]): RenderClient = {
    val openapiFilePath = CLP.parseArgument[String](commandArgs, 0)
    val renderer = CLP.parseArgument[String](commandArgs, 1)
    val renderToFilePath = if (commandArgs.length >= 3) Some(CLP.parseArgument[String](commandArgs, 2)) else None
    val openapiFile = Path.of(openapiFilePath).toAbsolutePath
    val renderToFile = renderToFilePath.map { str => Path.of(str).toAbsolutePath }
    val packageOpt = namedArgs.get("package").flatMap(_.headOption)
    val additionalImports = namedArgs.getOrElse("additionalImport", Vector.empty)
    RenderClient(openapiFile, renderer, renderToFile, packageOpt, additionalImports)
  }

}
