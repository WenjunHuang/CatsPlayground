package reader

import cats.data.Reader
import cats.syntax.applicative._

case class Cat(name: String, favoriteFood: String)

object CatsReader extends App {
  val catName: Reader[Cat, String] = Reader(cat => cat.name)
  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")
  val feedKitty: Reader[Cat, String] = Reader(
    cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] = for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed"

  println(greetAndFeed(Cat("Garfield", "lasagne")))
  println(greetAndFeed(Cat("Heathcliff", "junk food")))
  println(greetKitty.run(Cat("Healthcliff", "junk food")))

  type DbReader[T] = Reader[Db, T]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      passwordOk <- username.map { username =>
                     checkPassword(username, password)
                   }.getOrElse(false.pure[DbReader])
    } yield passwordOk

  val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
  val passwords =
    Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")

  val db = Db(users, passwords)
  checkLogin(1, "zerocool").run(db)
  checkLogin(4, "davinci").run(db)
}

case class Db(usernames: Map[Int, String], passwords: Map[String, String])
