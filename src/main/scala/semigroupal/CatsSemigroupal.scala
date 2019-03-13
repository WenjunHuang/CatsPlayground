package semigroupal
import cats.{ Monoid, Semigroupal }
import cats.instances.boolean._
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.instances.option._
import cats.instances.future._
import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.instances.invariant._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.language.higherKinds

case class Cat(name: String, born: Int, color: String)
case class Dog(name: String, dick: Option[Boolean])
case class People(name: String, yearOfBirth: Int, favoriteFoods: List[String])

object CatsSemigroupal extends App {
  val s1 = Semigroupal[Option].product(Some(123), Some("abc"))
  val s2 = Semigroupal[Option].product(None, Some("abc"))
  val s3 = Semigroupal.tuple3(Option(1), Option(2), Option(3))
  val s4 = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)

  println(s1)
  println(s2)
  println(s3)

  (Option(123), Option("abc")).tupled

  (Option(123), Option("abc"), Option(true)).tupled

  (Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply)

  val snoopy = (Option("Snoopy"), Option(None)).mapN(Dog.apply)

  println(snoopy)

  val add: (Int, Int) => Int = (a, b) => a + b
  (Option(1), Option(2)).mapN(add)

  val tupleToPeople: (String, Int, List[String]) => People = People.apply
  val peopleToTuple: People => (String, Int, List[String]) = cat =>
    (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val peopleMonoid: Monoid[People] =
    (Monoid[String], Monoid[Int], Monoid[List[String]])
      .imapN(tupleToPeople)(peopleToTuple)

  val garfield = People("Garfield", 1978, List("Lasagne"))
  val heathcliff = People("Heathcliff", 1988, List("Junk Food"))

  println(garfield |+| heathcliff)

  val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))
  Await.result(futurePair, 1.second)

  val futurePeople = (
    Future("Garfield"),
    Future(1978),
    Future(List("Lasagne"))
  ).mapN(People.apply)

  println(Await.result(futurePeople,1.second))
}
