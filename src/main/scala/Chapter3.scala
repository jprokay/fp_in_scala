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

  object Tree {

    sealed trait Tree[+A]

    case class Leaf[A](value: A) extends Tree[A]

    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    def size[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }
    }

    def maximum(tree: Tree[Int]): Int = {
      tree match {
        case Leaf(value) => value
        case Branch(l, r) => maximum(l) max maximum(r)
      }
    }

    def depth[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
      }
    }

    def map[A, B](as: Tree[A])(f: A => B): Tree[B] = {
      as match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }
    }

    def fold[A, B](as: Tree[A])(f: A => B)(g: (B, B) => B): B = {
      as match {
        case Leaf(v) => f(v)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      }
    }

    def sizeWithFold[A](tree: Tree[A]): Int = {
      fold(tree)(_ => 1)(1 + _ + _)
    }

    def maxWithFold(tree: Tree[Int]): Int = {
      fold[Int, Int](tree)(a => a)(_ max _)
    }

    def depthWithFold[A](tree: Tree[A]): Int = {
      fold[A, Int](tree)(_ => 0)((d1, d2) => 1 + (d1 max d2))
    }

    def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
    }
  }
}
