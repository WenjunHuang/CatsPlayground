package shapeless1
import shapeless._
import shapeless1.CsvEncoder._

object RecursiveTypes extends App {
  sealed trait Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit def hlistEncoder[H, T <: HList](
      implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = createEncoder {
    case h :: t =>
      hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }

  implicit def coproductEncoder[H, T <: Coproduct](
      implicit hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](
      implicit gen: Generic.Aux[A, R],
      rEncoder: Lazy[CsvEncoder[R]]): CsvEncoder[A] = createEncoder { value =>
    rEncoder.value.encode(gen.to(value))
  }


}
