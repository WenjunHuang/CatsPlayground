package cats.fold

object Simple extends App {

  def map[A, B](list: List[A])(func: A => B) =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) :: accum
    }

  def flatMap[A, B](list: List[A])(func: A => List[B]) =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) ::: accum
    }

  println(flatMap(List(1, 2, 3, 4)) { item =>
    List(item, item * 2, item * 3)
  })

}
