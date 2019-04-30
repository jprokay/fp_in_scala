import org.scalatest.{Matchers, WordSpec}
import Chapter4._


class Chapter4Test extends WordSpec with Matchers{
  "Chapter 4" should {
    "4.1 Option methods" in {
      val opt = Some(42)
      val none = None

      opt.map(_.toString) should be(Some("42"))
      none.map(_.toString) should be(None)

      opt.getOrElse(50) should be(42)
      none.getOrElse(50) should be(50)

      opt.orElse(Some(50)) should be(opt)
      none.orElse(Some(50)) should be(Some(50))

      opt.filter(_ % 5 == 0) should be(None)
      opt.filter(_ % 2 == 0) should be(opt)
      none.filter(_ == Some(42)) should be(None)
    }
  }

  "4.5 traverse" in {

    //traverse(List("1", "2", "3", "4"))(x => Try(x.toInt)) should be(List(1, 2, 3, 4))
  }
}
