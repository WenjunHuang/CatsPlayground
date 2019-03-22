package cats.semigroupal
import cats.data.Validated
import cats.instances.either._
import cats.instances.future._
import cats.instances.int._
import cats.instances.invariant._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.semigroup._
import cats.syntax.validated._
import cats.syntax.either._
import cats.{ Monad, Monoid, Semigroupal }

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.util.Try

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

  println(Await.result(futurePeople, 1.second))

  Semigroupal[List].product(List(1, 2), List(3, 4))

  type ErrorOr[A] = Either[Vector[String], A]
  Semigroupal[ErrorOr].product(
    Left(Vector("Error 1")),
    Left(Vector("Error 2"))
  )

  // implement product in terms of flatMap
  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b)

  type AllErrorsOr[A] = Validated[List[String], A]
  val validated = Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )

  println(validated)

  val v = Validated.Valid(123)
  val i = Validated.Invalid(List("Badness"))

  val v1 = Validated.valid[List[String], Int](123)
  val i1 = Validated.invalid[List[String], Int](List("Badness"))

  123.valid[List[String]]
  List("Badness").invalid[Int]

  Validated.catchOnly[NumberFormatException]("foo".toInt)
  Validated.catchOnly[ArithmeticException](1 / 0)
  Validated.catchNonFatal(sys.error("Badness"))

  type AllStringErrorsOr[A] = Validated[String, A]
  Semigroupal[AllStringErrorsOr]

  println(
    (
      "Error 1".invalid[Int],
      "Error 2".invalid[Int]
    ).tupled)

  println(
    (
      Vector(404).invalid[Int],
      Vector(500).invalid[Int]
    ).tupled)

  import cats.data.NonEmptyVector
  (
    NonEmptyVector.of("Error 1").invalid[Int],
    NonEmptyVector.of("Error 2").invalid[Int]
  ).tupled

  123.valid.map(_ * 100)
  "?".invalid.leftMap(_.toString)

  123.valid[String].bimap(_ + "!", _ * 100)
  "?".invalid[Int].bimap(_ + "!", _ * 100)

  // exercise
  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name).toRight(List(s"$name field not specified"))

  def parseInt(name: String)(data: String): FailFast[Int] =
    Either
      .catchOnly[NumberFormatException](data.toInt)
      .leftMap(_ => List(s"$name must be an integer"))

  def nonBlank(name: String)(data: String): FailFast[String] =
    Right(data)
      .ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def nonNegative(name: String)(data: Int): FailFast[Int] =
    Right(data)
      .ensure(List(s"$name must be non-negative"))(_ >= 0)

  def readName(data: FormData): FailFast[String] =
    getValue("name")(data)
      .flatMap(nonBlank("name"))

  def readAge(data: FormData): FailFast[Int] =
    getValue("age")(data)
      .flatMap(nonBlank("age"))
      .flatMap(parseInt("age"))
      .flatMap(nonNegative("age"))

  case class User(name: String, age: Int)
  def readUser(data: FormData): FailSlow[User] =
    (readName(data).toValidated, readAge(data).toValidated).mapN(User.apply)

  println(readUser(Map("name" -> "Dave", "age" -> "37")))
  println(readUser(Map("age" -> "-1")))
}
