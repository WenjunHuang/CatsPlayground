package cats.fold
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.instances.stream._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.{ Applicative, Eval, Foldable, Monad }

object CatsFoldable extends App {
  val ints = List(1, 2, 3)
  Foldable[List].foldLeft(ints, 0)(_ + _)

  val maybeInt = Option(123)
  Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

  def bigData = (1 to 100000).toStream
  // will stack overflow
//  bigData.foldRight(0L)(_ + _)

  val eval: Eval[Long] = Foldable[Stream].foldRight(bigData, Eval.now(0L)) {
    (num, eval) =>
      eval.map(_ + num)
  }
  println(eval.value)

  println(Foldable[List].combineAll(List(1, 2, 3)))

  println(Monad[List].flatMap(List(1, 2, 3))(it => List(it.toString)))
  Foldable[List].foldMap(List(1, 2, 3))(_.toString)

  val anotherInts = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(anotherInts)

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(
      func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]:Applicative,B](list:List[F[B]]):F[List[B]] =
    listTraverse(list)(identity)
}
