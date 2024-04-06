package net.girkin.ardeas.scala

import net.girkin.ardeas.Model._

trait ClientRenderer {
  def renderClient(api: Api, packageName: Option[String], additionalImportPackages: Iterable[String]): String
}
