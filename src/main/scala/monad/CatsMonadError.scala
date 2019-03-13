package monad

import cats.MonadError
import cats.instances.either._

object CatsMonadError extends App {
  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]
  val success = monadError.pure(42)
  val failure = monadError.raiseError("Badness")
}
