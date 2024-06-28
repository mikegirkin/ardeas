package io.github.mikegirkin.ardeas.scala

import io.github.mikegirkin.ardeas.Model._

trait ClientRenderer {
  def renderClient(api: Api, packageName: Option[String], additionalImportPackages: Iterable[String]): String
}
