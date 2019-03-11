package monad

import cats.Monad
import cats.instances.option._
import cats.instances.list._
import cats.instances.vector._
import cats.instances.future._
import scala.concurrent._
import scala.concurrent.duration._
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap
import cats.Id

object CatsMonad extends App {
  val opt1 = Monad[Option].pure(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  val opt3 = Monad[Option].map(opt2)(a => 100 * a)

  val list1 = Monad[List].pure(3)
  // cat
  val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))

  Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))

  // scala
  val seq1 = Seq(1, 2, 3).flatMap(a => Seq(a, a * 10))

  println(seq1)

  import scala.concurrent.ExecutionContext.Implicits.global

  val fm = Monad[Future]
  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
  Await.result(future, 1.seconds)

  sumSquare(Option(3), Option(4))

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  def forSumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

}
