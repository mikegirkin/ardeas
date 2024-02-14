package net.girkin.ardeas

import java.net.URI
import net.girkin.ardeas.scala.entities.*
import org.slf4j.LoggerFactory

@main def main(args: String*): Unit = {
  val logger = LoggerFactory.getLogger("Main")

  for {
    command <- CLI.getCommand(args.toArray)
    _ = logger.info(s"Processing command: ${command}")
  } yield {
    command.run()
  }
}
