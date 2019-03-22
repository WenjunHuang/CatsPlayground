package cats.monad

import cats.Monad
import cats.syntax.monad._

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object CatsMonadForTree extends App {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad = new Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(left, right) =>
          Branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(value) =>
          f(value)
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      f(a) match {
        case Branch(left, right) =>
          Branch(
            flatMap(left) {
              case Left(l)  => tailRecM(l)(f)
              case Right(l) => pure(l)
            },
            flatMap(right) {
              case Left(r)  => tailRecM(r)(f)
              case Right(r) => pure(r)
            }
          )
        case Leaf(Left(value))  => tailRecM(value)(f)
        case Leaf(Right(value)) => Leaf(value)
      }

    override def pure[A](x: A): Tree[A] = Leaf(x)
  }

}
