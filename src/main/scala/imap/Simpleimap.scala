package imap

object IMap extends App {

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  println(encode(123))
  println(encode(contramap.Box(123)))
}

trait Codec[A] {
  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(value: B): String = Codec.this.encode(enc(value))

    override def decode(value: String): B = dec(Codec.this.decode(value))
  }
}

object Codec {
  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String) = value

    override def decode(value: String) = value
  }

  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec:Codec[Double] = stringCodec.imap(_.toDouble,_.toString)

  implicit def boxCodec[A: Codec]: Codec[contramap.Box[A]] = stringCodec.imap({ str => contramap.Box(implicitly[Codec[A]].decode(str)) },
    { box =>
      implicitly[Codec[A]].encode(box.value)
    })
}

case class Box[A](value: A)

