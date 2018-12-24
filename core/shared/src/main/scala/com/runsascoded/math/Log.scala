package com.runsascoded.math

object Log {
  implicit class Ops(val n: Int) extends AnyVal {
    /**
     * Return the largest power of `b` that is less than or equal to [[n]], as well as the base-b log of that power
     */
    def log(b: Int): (Int, Int) = {
      var p = 1
      var r = 0
      var next = p * b
      while (next <= n) {
        p = next
        next *= b
        r += 1
      }
      (p, r)
    }
  }
  trait syntax {
    @inline implicit def toLogOps(n: Int): Ops = Ops(n)
  }
}
