package shapeless1

import shapeless._
import shapeless1.CsvEncoder._

sealed trait Shape
final case class Rectangle(width:Double,height:Double) extends Shape
final case class Circle(radius:Double) extends Shape

object TypeClassGenerator extends App {
  case class Employee(name: String, number: Int, manger: Boolean)




  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

  reprEncoder.encode("abc" :: 123 :: true :: HNil)

  implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
    val gen = Generic[IceCream]
    val enc = CsvEncoder[gen.Repr]
    createEncoder(iceCream => enc.encode(gen.to(iceCream)))
  }

  implicit def genericEncoder[A, R](implicit gen: Generic[A] { type Repr = R }, enc: CsvEncoder[R])
    : CsvEncoder[A] = createEncoder(a => enc.encode(gen.to(a)))

}
