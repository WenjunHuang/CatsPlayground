package either

import cats.syntax.either._

object CatsEither extends App {

  val a = 3.asRight[String]

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative.Stopping!")
      }
    }

  val result1 = Either.catchOnly[NumberFormatException]("foo".toInt)

  "Error".asLeft[Int].getOrElse(0)
  "Error".asLeft[Int].orElse(2.asRight[String])

  for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "DIV0".asLeft[Int] else (a / b).asRight[String]
  } yield c * 100
}

sealed trait LoginError extends Product with Serializable

final case class UserNotFound(username:String) extends LoginError