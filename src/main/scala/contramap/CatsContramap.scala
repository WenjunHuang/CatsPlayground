package contramap

import cats.Contravariant
import cats.Show
import cats.instances.string._ // for Monoid
import cats.syntax.contravariant._

object CatsContramap extends App{
  val showString = Show[String]
  val showSymbol = Contravariant[Show].contramap(showString)((sym:Symbol)=>s"'${sym.name}")
  showSymbol.show('dave)

  showString.contramap[Symbol](_.name).show('dave)

}
