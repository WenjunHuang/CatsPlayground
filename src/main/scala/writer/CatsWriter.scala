package writer

import cats.data.Writer
import cats.instances.vector._ // for Monoid
import cats.syntax.applicative._ // for pure
import cats.syntax.writer._

object CatsWriter extends App {
  Writer(Vector(
    "It was the best of times",
    "it was the worst of times"
  ), 1859)

  type Logged[A] = Writer[Vector[String], A]
  123.pure[Logged]

  Vector("msg1", "msg2", "msg3").tell
  val a = 123.writer(Vector("msg1", "msg2", "msg3"))

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )
  writer1.run
  println(writer2.run._1.mkString(","))
}
