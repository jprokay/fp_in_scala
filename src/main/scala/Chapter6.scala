object Chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, rng2) = rng.nextInt
      (if (i < 0) -(i + 1) else i, rng2)
    }

    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = RNG.nonNegativeInt(rng)
      ((i / Int.MaxValue).toDouble, r)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r2) = rng.nextInt
      val (d, _) = double(rng)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), r) = intDouble(rng)
      ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r2) = double(rng)
      val (d2, r3) = double(r2)
      val (d3, r4) = double(r3)
      ((d1, d2, d3), r4)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      (0 to count).foldLeft[(List[Int], RNG)]({ val (i, r2) = rng.nextInt; (List(i), r2) })((l, _) => {
        val list = l._1
        val r3 = l._2
        val (i2, r4) = r3.nextInt
        (list ++ List(i2), r4)
      })
    }
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
}
