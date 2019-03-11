package contramap

object Contramap extends App {
  implicit val stringPrintable: Printable[String] = (value: String) => s""""$value""""

  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"

  def format[A: Printable](value: A): String = implicitly[Printable[A]].format(value)

  println(format("Hello"))
  println(format(true))
  println(format(Box("Hello")))
  println(format(Box(true)))
}

trait Printable[A] {
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = (value: B) => format(func(value))
}

final case class Box[A](value: A)

object Box {
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)
}
