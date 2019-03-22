package cats.state

import cats.Monad
import cats.syntax.monad._
import cats.state.MyState.ListIntState

case class MyState[S, A](run: S => (A, S))

object MyState {

  type ListIntState[A] = MyState[List[Int], A]

  implicit def listInStateMonad: Monad[ListIntState] = new Monad[ListIntState] {
    override def flatMap[A, B](fa: ListIntState[A])(f: A => ListIntState[B]): ListIntState[B] =
      MyState[List[Int], B](s => {
        val (a, s1) = fa.run(s)
        f(a).run(s1)
      })

    override def pure[A](x: A): ListIntState[A] = MyState[List[Int], A](s => (x, s))

    override def tailRecM[A, B](a: A)(f: A => ListIntState[Either[A, B]]): ListIntState[B] = ???
  }
}

object SimpleState extends App {}
