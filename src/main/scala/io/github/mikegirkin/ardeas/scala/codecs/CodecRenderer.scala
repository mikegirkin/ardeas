package io.github.mikegirkin.ardeas.scala.codecs

import cats.data.ValidatedNec
import io.github.mikegirkin.ardeas.Model
import io.github.mikegirkin.ardeas.scala.NotYetImplemented

trait CodecRenderer {
  def renderCodecsForModels(api: Model.Api, `package`: Option[String], additionalImportPackages: Iterable[String]): ValidatedNec[NotYetImplemented, String]
}
