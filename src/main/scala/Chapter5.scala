object Chapter5 {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => List()
      case Cons(h, t) => List(h()) ::: t().toList
    }

    def take(n: Int): Stream[A] = {
      this match {
        case Empty => Stream.empty
        case Cons(h, t) =>
          if (n > 0)
            Cons(h, () => t().take(n - 1))
          else
            Stream.empty
      }
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      this match {
        case Empty => Stream.empty
        case Cons(h, t) =>
          if (p(h()))
            Cons(h, () => t().takeWhile(p))
          else
            t().takeWhile(p)
      }
    }

    def exists(p: A => Boolean): Boolean = {
      this match {
        case Cons(h, t) => p(h()) || t().exists(p)
        case _ => false
      }
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def foldExists(p: A => Boolean): Boolean = this.foldRight(false)((h, t) => p(h) || t)

    def forAll(p: A => Boolean): Boolean = this.foldRight(true)((h, t) => p(h) && t)

   // def map[B](f: A => B): Stream[B] = this.foldRight(Stream.empty[B])((h, t) => Cons(() => f(h), () => t.map(f)))

    def filter(f: A => Boolean): Stream[A] = this.foldRight(Stream.empty[A])((h, t) => if (f(h)) Cons(() => h, () => t.filter(f)) else t.filter(f))

    //def append(v: Stream[A]): Stream[A] = this.foldRight(Stream.empty[A])((h, t) => Cons(() => h, () => t.append(v)))
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
