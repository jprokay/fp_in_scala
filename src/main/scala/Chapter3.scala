import scala.annotation.tailrec

object Chapter3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](h: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def tail[A](as: List[A]): List[A] = as match {
      case Cons(_, Cons(h, tail)) => Cons(h, tail)
      case Cons(_, Nil) => Nil
      case Nil => Nil
    }

    def setHead[A](as: List[A], newHead: A): List[A] = as match {
      case Nil => List(newHead)
      case Cons(h, tail) => Cons(newHead, Cons(h, tail))
    }

    @tailrec
    def drop[A](as: List[A], n: Int): List[A] = {
      if (n == 0) as
      else as match {
        case Nil => drop(Nil, 0)
        case Cons(_, t) => drop(t, n - 1)
      }
    }

    def dropWhile[A](as: List[A], f: A => Boolean): List[A] = {
      as match {
        case Nil => Nil
        case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, dropWhile(t, f))
      }
    }

    def init[A](as: List[A]): List[A] = {
      as match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, tail) => Cons(h, init(tail))
      }
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
    }

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }
    }

    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_ , b: Int) => 1 + b)
    }

    def leftSum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def leftProduct(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    def leftLength[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

    def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)((a, b) => Cons(a, b))

    def concat[A](las: List[List[A]]): List[A] = {
      foldRight(las, List[A]())(append)
    }

    def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((b: List[A], a: A) => setHead(b, a))

    def addOne(is: List[Int]): List[Int] = is match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }

    def map[A, B](as: List[A])(f: A => B): List[B] = as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
      foldRight(as, List[B]())((a, b) => append(f(a), b))
    }

    def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

    def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] = {
      (l, r) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      }
    }
  }
}
