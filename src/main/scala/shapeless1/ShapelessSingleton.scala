package shapeless1

import shapeless._
import shapeless.labelled.FieldType
import shapeless.syntax.singleton._

object ShapelessSingleton extends App {
  trait Cherries

  val someNumber = 123
  val numCherries = "numCherries" ->> someNumber

  def getFieldName[K, V](value: FieldType[K, V])(
      implicit witness: Witness.Aux[K]): K = witness.value

  def getFieldValue[K,V](value:FieldType[K,V]):V = value

  println(getFieldName(numCherries))
  println(getFieldValue(numCherries))

  val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil

}
