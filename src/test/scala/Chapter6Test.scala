import Chapter6.{RNG, SimpleRNG}
import org.scalatest.{Matchers, WordSpec}

class Chapter6Test extends WordSpec with Matchers {
  "Chapter 6" should {
    "6.1 RNG.nextInt and RNG.nonNegativeInt" in {
      val rng = SimpleRNG(42)
      val (n1, rng2) = rng.nextInt
      n1 should be(16159453)

      val (n2, rng3) = rng2.nextInt
      n2 should be(-1281479697)

      val (nonNegative, rng4) = RNG.nonNegativeInt(rng2)
      nonNegative should be(n2 * -1)
    }

    "6.2 RNG.double(rng: RNG) returns a double between 0 and 1" in {
      val rng = SimpleRNG(42)
      val (d, r2) = RNG.double(rng)
      d should be((16159453 / Int.MaxValue).toDouble)
    }

    "6.3 RNG.ints(count: Int) returns a number of random ints" in {
      val rng = SimpleRNG(42)
      val rs = RNG.ints(10)(rng)
      rs should be(RNG.ints(10)(rng))
    }
  }
}
