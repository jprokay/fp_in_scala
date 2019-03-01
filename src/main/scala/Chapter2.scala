import scala.annotation.tailrec

object Chapter2 {

  def fib(n: Int): Int = {
    @tailrec
    def go(m: Int, current: Int, previous: Int): Int = {
      if (m <= 0) current
      else go(m - 1, current + previous, current)
    }

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def check(m: Int, truthiness: Boolean): Boolean = {
      if (m + 1 >= as.length) truthiness
      else check(m + 1, truthiness && ordered(as(m), as(m + 1)))
    }

    check(0, truthiness = true)
  }

  def currying[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
