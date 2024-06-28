package io.github.mikegirkin.ardeas

import cats.Id
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Kleisli, NonEmptyChain, Reader, Validated, ValidatedNec}
import cats.syntax.either.*
import cats.syntax.validated.*
import io.swagger.v3.oas.models.media.Schema
import io.github.mikegirkin.ardeas.parser.ParsingError

object ValidatedUtils {
  def invalidNec[E](left: E) = {
    Invalid(left).toValidatedNec[E, Nothing]
  }

  def validNec[A](right: A) = {
    Valid(right).toValidatedNec[Nothing, A]
  }
}

object ParserKleisliUtils {
  extension[In, Out1, Out2] (parser: Kleisli[Option, In, ValidatedNec[ParsingError, Out1]]) {
    def orTryParseWith(otherParser: Kleisli[Option, In, ValidatedNec[ParsingError, Out2]]): Kleisli[Option, In, ValidatedNec[ParsingError, Out1 | Out2]] =
      Kleisli { schema =>
        parser(schema).orElse(otherParser(schema))
      }

    def orParseWith(otherParser: Reader[In, ValidatedNec[ParsingError, Out2]]): Reader[In, ValidatedNec[ParsingError, Out1 | Out2]] =
      Reader { in =>
        parser(in).getOrElse(otherParser(in))
      }

    def closeWithError(error: ParsingError) = {
      parser.mapF(_.getOrElse(
        error.invalidNec
      ))
    }
  }
}