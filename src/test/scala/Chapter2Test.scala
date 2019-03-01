import org.scalatest.{Matchers, WordSpec}
import Chapter2._

class Chapter2Test extends WordSpec with Matchers {

  "Chapter 2" should {

    "2.1 Fibonacci" in {
      fib(0) should be(0)
      fib(1) should be(1)
      fib(2) should be(1)
      fib(3) should be(2)
      fib(10) should be(55)
      fib(20) should be(6765)
    }

    "2.2 isSorted[A]" in {
      val ints = Array(1, 2, 3, 4, 5, 6)
      val asc = { (a: Int, b: Int) => a < b }
      isSorted(ints, asc) should be(true)
      isSorted[Int](ints, _ > _) should be(false)

      val ss = Array("a", "bbb", "cccc", "ddddd")
      isSorted[String](ss, _.length < _.length) should be(true)
      isSorted[String](ss, _.length > _.length) should be(false)

      isSorted[Int](Array(), asc) should be(true)
      isSorted[Int](Array(1), asc) should be(true)
    }

    "2.3 currying[A, B, C]" in {
      val sum = currying[Int, Int, Int]((a, b) => a + b)
      sum(2)(2) should be(4)

      val logline = currying[String, Int, String]((a, b) => s"$b: $a")
      logline("Arbitrary")(2) should be("2: Arbitrary")
    }

    "2.4 uncurrying[A, B, C]" in {
      val sum = currying[Int, Int, Int]((a, b) => a + b)
      uncurry(sum)(2,2) should be(4)
      val logline = currying[String, Int, String]((a, b) => s"$b: $a")
      uncurry(logline)("Arbitrary", 2) should be("2: Arbitrary")
    }

    "2.5 compose" in {
      val stringToInts = compose((b: Array[String]) => b.map(_.toInt), (a: String) => a.split(","))
      stringToInts("1,2,3,4,5") should be(Array(1,2,3,4,5))
    }
  }
}
