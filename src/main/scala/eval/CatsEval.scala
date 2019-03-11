package eval

import cats.Eval

object CatsEval extends App {
  val now = Eval.now {
    println("Computing X")
    math.random
  }
  val later = Eval.later(math.random + 2000)
  val always = Eval.always(math.random + 3000)

  for (i <- 0 to 10) {
    println(s"now:${now.value}")
    println(s"later:${later.value}")
    println(s"always:${always.value}")
  }

  val greeting = Eval.always {
    println("Step 1");
    "Hello"
  }.map { str => println("Step 2"); s"$str world" }

  println(greeting.value)

  val ans = for {
    a <- Eval.now {
      println("Calculating A");
      40
    }
    b <- Eval.always {
      println("Calculating B");
      2
    }
  } yield {
    println("Adding A and B")
    a + b
  }

  ans.value
  ans.value

  val saying = Eval
    .always {
      println("Step 1");
      "The cat"
    }
    .map { str => println("Step 2"); s"$str sat on" }
    .memoize
    .map { str => println("Step 3"); s"$str the mat" }
  saying.value
  saying.value

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  println(factorial(50000).value)

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  println(foldRight((1 to 100000).toList,0L)(_ + _))
}
