package io.github.mikegirkin.ardeas

import org.slf4j.LoggerFactory

object Main {
  def main(args: Array[String]): Unit = {
    val logger = LoggerFactory.getLogger("Main")

    for {
      command <- CLI.getCommand(args.toSeq)
      _ = logger.info(s"Processing command: ${command}")
    } yield {
      command.run()
    }
  }
}