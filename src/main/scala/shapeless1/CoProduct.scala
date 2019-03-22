package shapeless1
import shapeless._
import shapeless1.CsvEncoder._

object CoProduct extends App {
  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

//  implicit val rectangleEncoder: CsvEncoder[Rectangle] = {
//    val gen = Generic[Rectangle]
//    val enc = CsvEncoder[gen.Repr]
//    createEncoder(rectangle => enc.encode(gen.to(rectangle)))
//  }
//
//  implicit val circleEncoder: CsvEncoder[Circle] = {
//    val gen = Generic[Circle]
//    val enc = CsvEncoder[gen.Repr]
//    createEncoder(circle => enc.encode(gen.to(circle)))
//  }
//
//  implicit val shapeEncoder: CsvEncoder[Shape] = {
//    val gen = Generic[Shape]
//    val enc = CsvEncoder[gen.Repr]
//    createEncoder(shape => enc.encode(gen.to(shape)))
//  }

  implicit val cnilEncoder: CsvEncoder[CNil] = createEncoder(
    cnil => throw new Exception("Inconceivable"))

  implicit def coproductEncoder[H, T <: Coproduct](
      implicit hEncoder: CsvEncoder[H],
      tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](implicit gen: Generic.Aux[A, R],
                                    rEncoder: CsvEncoder[R]): CsvEncoder[A] =
    createEncoder { value =>
      rEncoder.encode(gen.to(value))
    }

  val shapes: List[Shape] = List(
    Rectangle(3.0, 4.0),
    Circle(1.0)
  )

  println(writeCsv(shapes))

}
