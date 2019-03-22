package shapeless1
import shapeless._

case class Employee(name: String, number: Int, manager: Boolean)
case class IceCream(name: String, numCherries: Int, inCone: Boolean)
case class BigData(a: Int,
                   b: Int,
                   c: Int,
                   d: Int,
                   e: Int,
                   f: Int,
                   g: Int,
                   h: Int,
                   i: Int,
                   j: Int,
                   k: Int,
                   l: Int,
                   m: Int,
                   n: Int,
                   o: Int,
                   p: Int,
                   q: Int,
                   r: Int,
                   s: Int,
                   t: Int,
                   u: Int,
                   v: Int,
                   w: Int)

object Simple extends App {
  def employeeCsv(e: Employee): List[String] =
    List(e.name, e.number.toString, e.manager.toString)

  def iceCreamCsv(c: IceCream): List[String] =
    List(c.name, c.numCherries.toString, c.inCone.toString)

  val genericEmployee = Generic[Employee].to(Employee("Dave", 123, false))
  val genericIceCream = Generic[IceCream].to(IceCream("Sundae", 1, false))

  def genericCsv(gen: String :: Int :: Boolean :: HNil): List[String] =
    List(gen(0), gen(1).toString, gen(2).toString)

  genericCsv(genericEmployee)
  genericCsv(genericIceCream)

  val product = "Sunday" :: 1 :: false :: HNil
  val first = product.head
  val second = product.tail.head
  val rest = product.tail.tail

  val newProduct = 42L :: product

  val iceCreamGen = Generic[IceCream]
  val iceCream = IceCream("Sundae", 1, false)
  val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

  Generic[BigData].from(
    Generic[BigData].to(
      BigData(
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23
      )))

  case class Red()
  case class Amber()
  case class Green()

  type Light = Red :+: Amber :+: Green :+: CNil

  val red:Light = Inl(Red())
}
