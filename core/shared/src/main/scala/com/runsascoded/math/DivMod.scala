package com.runsascoded.math

object DivMod {
  implicit class Ops(val n: Int) extends AnyVal {
    def /%(o: Int): (Int, Int) = (n / o, n % o)
  }

  trait syntax {
    @inline implicit def toDivMod(n: Int): Ops = Ops(n)
  }
}
