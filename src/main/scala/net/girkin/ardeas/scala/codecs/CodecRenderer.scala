package net.girkin.ardeas.scala.codecs

import cats.data.ValidatedNec
import net.girkin.ardeas.Model
import net.girkin.ardeas.scala.NotYetImplemented

trait CodecRenderer {
  def renderCodecsForModels(api: Model.Api, `package`: Option[String], additionalImportPackages: Iterable[String]): ValidatedNec[NotYetImplemented, String]
}
