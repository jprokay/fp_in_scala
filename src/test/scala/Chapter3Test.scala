import Chapter3.Tree.{Branch, Leaf}
import org.scalatest.{Matchers, WordSpec}
import Chapter3._

class Chapter3Test extends WordSpec with Matchers {

  "Chapter 3" should {

    "3.1 Case Matching" in {
      val matched = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }

      matched should be(3)
    }

    "3.2 Get tail of list" in {
      List.tail(List(1, 2, 3, 4, 5)) should be(List(2, 3, 4, 5))
      List.tail(Nil) should be(Nil)
      List.tail(List.tail(List(2, 3))) should be(Nil)
      List.tail(List.tail(List.tail(List(2, 3)))) should be(Nil)
    }

    "3.3 Set new head of list" in {
      List.setHead(List(2, 3, 4, 5), 1) should be(List(1, 2, 3, 4, 5))
      List.setHead(Nil, 3) should be(List(3))
      List.setHead(List(2), 1) should be(List(1, 2))
    }

    "3.4 Drop n elements from List" in {
      List.drop(List(1, 2, 3, 4, 5), 3) should be(List(4, 5))
      List.drop(Nil, 3) should be(Nil)
      List.drop(List(1, 2, 3), 4) should be(Nil)
    }

    "3.5 Drop elements matching predicate" in {
      val evens = (a: Int) => a % 2 == 0
      List.dropWhile(List(1, 2, 3, 4, 5), evens) should be(List(1, 3, 5))
      List.dropWhile(Nil, evens) should be(Nil)
    }

    "3.6 Drop the last element from the list" in {
      List.init(List(1, 2, 3, 4)) should be(List(1, 2, 3))
      List.init(Nil) should be(Nil)
      List.init(List(1)) should be(Nil)
    }

    "3.9 Get the length of a list" in {
      List.length(List(1, 2, 3, 4)) should be(4)
      List.length(Nil) should be(0)
    }

    "3.11 Methods implemented with fold left equivalent to initial impl" in {
      val ints = List(1, 2, 3, 4)
      List.sum(ints) should be(List.leftSum(ints))

      val doubles = List(1.0, 2.0, 3.0, 4.0)
      List.product(doubles) should be(List.leftProduct(doubles))

      List.length(ints) should be(List.leftLength(ints))
    }

    "3.12 Reverse" in {
      val ints = List(1, 2, 3, 4)
      List.reverse(ints) should be(List(4, 3, 2, 1))
    }

    "3.14 Append" in {
      List.append(Nil, List(1)) should be(List(1))
      List.append(List(1, 2, 3, 4), List(5, 6, 7, 8)) should be(List(1, 2, 3, 4, 5, 6, 7, 8))
    }

    "3.15 Concat" in {
      List.concat(List(List(1, 2, 3, 4), List(5, 6, 7, 8))) should be(List(1, 2, 3, 4, 5, 6, 7, 8))
    }

    "3.16 Add One to a List of Ints" in {
      List.addOne(List(1, 2, 3, 4)) should be(List(2, 3, 4, 5))
    }

    "3.18 Map over a list" in {
      val list = List(1, 2, 3, 4)
      List.map(list)(_ + 1) should be(List.addOne(list))
    }

    "3.19 Filter" in {
      val list = List(1, 2, 3, 4, 5, 6)
      List.filter(list)(_ % 2 == 0) should be(List(2, 4, 6))

      List.filter(list)(_ % 2 == 1) should be(List(1, 3, 5))
    }

    "3.20 flatMap" in {
      List.flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
    }

    "3.21 Filter with flatMap" in {
      val list = List(1, 2, 3, 4, 5, 6)
      List.filter(list)(_ % 2 == 0) should be(List(2, 4, 6))

      List.filter(list)(_ % 2 == 1) should be(List(1, 3, 5))
    }

    "3.22 - 3.23 zipWith" in {
      val list1 = List(1, 2, 3, 4)
      val list2 = List(1, 2, 3, 4)
      List.zipWith(list1, list2)(_ + _) should be(List(2, 4, 6, 8))
    }

    "3.25 Tree.size" in {
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(5), Branch(Leaf(6), Leaf(7))))))
      Tree.size(t) should be(13)
    }

    "3.26 Tree.maximum" in {
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(5), Branch(Leaf(4), Leaf(3))))))
      Tree.maximum(t) should be(5)
    }

    "3.27 Tree.depth" in {
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(5), Branch(Leaf(6), Leaf(7))))))
      Tree.depth(t) should be(5)
    }

    "3.28 Tree.map" in {
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
      Tree.map(t)(_ + 10) should be(Branch(Branch(Leaf(11), Leaf(12)), Branch(Leaf(13), Leaf(14))))
    }

    "3.29 Tree.fold" in {
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
      Tree.mapWithFold(t)(_ + 10) should be(Branch(Branch(Leaf(11), Leaf(12)), Branch(Leaf(13), Leaf(14))))

      val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(5), Branch(Leaf(6), Leaf(7))))))
      Tree.depthWithFold(t2) should be(5)

      val t3 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(5), Branch(Leaf(6), Leaf(7))))))
      Tree.sizeWithFold(t3) should be(13)
    }
  }
}
