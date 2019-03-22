package cats.imap

import cats.Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.invariant._ // for imap
import cats.syntax.semigroup._ // for |+|

object Catsimap extends App {
  implicit val symbolMonoid:Monoid[Symbol] = Monoid[String].imap(Symbol.apply)(_.name)

  'a |+| 'few |+| 'words

}
