package cats.fold
import cats.Applicative
import cats.instances.future._
import cats.instances.option._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent._
import scala.language.higherKinds

object CatsTraversing extends App {
  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

  val allUptimes: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)

  def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(
      func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  val l = listSequence(List(Vector(1, 2), Vector(3, 4)))
  println(l)

  def processOptions(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  println(processOptions(List(2, 4, 6)))
  println(processOptions(List(1, 2, 3)))

  import cats.data.Validated
  import cats.instances.list._

  type ErrorsOr[A] = Validated[List[String], A]

  def processValidated(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not event"))
      }
    }

  println(processValidated(List(2, 4, 6)))
  println(processValidated(List(1, 2, 3)))
}
