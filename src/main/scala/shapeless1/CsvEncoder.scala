package shapeless1
import shapeless._

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def apply[T: CsvEncoder] = implicitly[CsvEncoder[T]]

  def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
    (value: A) => func(value)

  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]) =
    values.map(value => encoder.encode(value).mkString(",")).mkString("\n")

  implicit val stringEncoder: CsvEncoder[String] = createEncoder(
    str => List(str))

  implicit val intEncoder: CsvEncoder[Int] = createEncoder(
    num => List(num.toString))

  implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder(
    bool => List(if (bool) "yes" else "no"))

  implicit val doubleEncoder: CsvEncoder[Double] = createEncoder(
    num => List(num.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder(hnil => Nil)

  implicit def hlistEncoder[H: CsvEncoder, T <: HList: CsvEncoder]
    : CsvEncoder[H :: T] =
    createEncoder {
      case h :: t =>
        implicitly[CsvEncoder[H]]
          .encode(h) ++ implicitly[CsvEncoder[T]].encode(t)
    }
}
