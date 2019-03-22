package shapeless1
import shapeless._
import shapeless.ops.hlist.{ IsHCons, Last }

object Types extends App {

  case class Vec(x: Int, y: Int)
  case class Rect(x: Vec, y: Vec)

  def getRepr[A](value: A)(implicit gen: Generic[A]) = gen.to(value)

  val repr1 = getRepr(Vec(1, 2))

  val last1 = Last[String :: Int :: HNil]
  val last2 = Last[Int :: String :: HNil]

  println(last1("foo" :: 123 :: HNil))
  println(last2(321 :: "bar" :: HNil))

  trait Second[L <: HList] {
    type Out
    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] { type Out = O }

    def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst

    implicit def hsingleSecond[H, T, Rest <: HList]: Aux[H :: T :: Rest, T] =
      new Second[H :: T :: Rest] {
        type Out = T
        def apply(l: H :: T :: Rest): Out = l.tail.head
      }
  }

  val second1 = Second[String :: Int :: HNil]
  println(second1("foo" :: 123 :: HNil))

  val second2 = Second[String :: Int :: Boolean :: HNil]
  println(second2("foo" :: 1231 :: true :: HNil))

  def lastField[A, R <: HList](input: A)(implicit
                                         gen: Generic.Aux[A, R],
                                         last: Last[R]): last.Out =
    last.apply(gen.to(input))

  lastField(Rect(Vec(1, 2), Vec(3, 4)))

  def getWrappedValue[A, Repr <: HList, Head, Tail <: HList](input: A)(
      implicit gen: Generic.Aux[A, Repr],
      isHCons: IsHCons.Aux[Repr, Head, HNil]): Head = gen.to(input).head

  the[Last[String :: Int :: HNil]]

  case class Wrapped(value:Int)

  getWrappedValue(Wrapped(100))
}
