package experiment
import cats.Id
import cats.effect.IO
import shapeless.the

trait Pet[A] {
  def name(a: A): String
  def renamed(a: A, newName: String): A
}
object Pet {
  implicit class PetOps[A](a: A)(implicit ev: Pet[A]) {
    def name = ev.name(a)
    def renamed(newName: String): A = ev.renamed(a, newName)
  }
}

// typeclass
trait YelpVoice[F[_],A] {
  def yelp(a: A): F[String]
}

// typeclass
// F表示某种effect
trait Luable[F[_], A] {
  def lu(a: A): F[A]
}

object Luable {
  implicit class LuableOps[F[_], A](a: A)(implicit ev: Luable[F, A]) {
    def lu() = ev.lu(a)
  }
}

// typeclass
// F表示某种effect
trait Liuable[F[_], A] {
  def liu(a: A): F[A]
  def throwABone(a: A): F[A]
}

object Liuable {
  implicit class LiuableOps[F[_], A](a: A)(implicit ev: Liuable[F, A]) {
    def liu() = ev.liu(a)
    def throwABone() = ev.throwABone(a)
  }
}

// data class
case class Dog(name: String)

object Dog {
  implicit val dogPetInstance: Pet[Dog] = new Pet[Dog] {
    override def name(a: Dog): String = a.name
    override def renamed(a: Dog, newName: String): Dog = a.copy(name = newName)
  }

  implicit def dogLiuableInstance: Liuable[IO, Dog] = new Liuable[IO, Dog] {
    override def liu(a: Dog): IO[Dog] = {
      val dogName = the[Pet[Dog]].name(a)
      IO(println(s"$dogName 一声不吭地跟在你后面，"))
        .map(_ => a)
    }

    override def throwABone(a: Dog): IO[Dog] = {
      val dogName = the[Pet[Dog]].name(a)
      IO(println(s"$dogName 高兴地追着骨头，过了一会，它叼回一位小姐姐"))
        .map(_ => a)
    }
  }
}

case class Cat(name: String)

object Cat {
  implicit val catPetInstance: Pet[Cat] = new Pet[Cat] {
    override def name(a: Cat): String = a.name
    override def renamed(a: Cat, newName: String): Cat = a.copy(name = newName)
  }

  implicit val catYelpInstance: YelpVoice[Id,Cat] = (_: Cat) => "meow"

  implicit def catLuableInstance: Luable[IO, Cat] = (a: Cat) => {
    val yelpVoice = the[YelpVoice[Id,Cat]].yelp(a)
    val catName = the[Pet[Cat]].name(a)
    IO(println(s"$catName 发出了舒服的 $yelpVoice 叫声"))
      .map(_ => a)
  }
}

object MultipleTypeClass extends App {

  import Luable._
  import Liuable._

  val dog = Dog("二哈")
  val cat = Cat("喵酱")

  val lu = cat.lu()

  val p = for {
    _ <- lu
    _ <- dog.liu()
    _ <- lu
    _ <- dog.throwABone()
  } yield ()
  p.unsafeRunSync()

}
