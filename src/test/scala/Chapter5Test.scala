import org.scalatest.{Matchers, WordSpec}
import Chapter5._

class Chapter5Test extends WordSpec with Matchers{
  "Chapter 5" should {
    "5.1 Stream.toList" in {
      Stream(1, 2, 3, 4, 5).toList should be(List(1, 2, 3, 4, 5))
      Stream().toList should be(List())
    }

    "5.2 Stream.take(n)" in {
      val stream = Stream(1, 2, 3, 4, 5)
      stream.take(1).toList should be(List(1))
      stream.take(0).toList should be(List())
      stream.take(4).toList should be(List(1, 2, 3, 4))
      stream.take(6).toList should be(List(1, 2, 3, 4, 5))
    }

    "5.3 Stream.takeWhile(p: A => Boolean)" in {
      val stream = Stream(1, 2, 3, 4, 5, 6)
      stream.takeWhile(_ % 2 == 0).toList should be(List(2, 4, 6))
    }

    "5.4 Stream.forAll(p: A => Boolean)" in {
      val evenStream = Stream(2, 4, 6)
      evenStream.forAll(_ % 2 == 0) should be(true)
      evenStream.forAll(_ % 2 == 1) should be(false)
    }

    "5.7 map, filter, append, flatmap" in {
      val stream = Stream(1, 2, 3, 4, 5, 6)
//      stream.map(_ + 10).toList should be(List(11, 12, 13, 14, 15, 16))
//      stream.map(_.toString).toList should be(List("1", "2", "3", "4", "5", "6"))

      stream.filter(_ % 2 == 0).toList should be(List(2, 4, 6))
    }
  }

}
