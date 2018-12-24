package com.runsascoded.hilbert.three

import runsascoded.math._

trait to {

  def Δ(% : Int): Point =
    //( (n&1)^(n&2), (n&2)^(n&4), n&4 )
    % match {
      case 0 ⇒ (0, 0, 0)
      case 1 ⇒ (1, 0, 0)
      case 2 ⇒ (1, 1, 0)
      case 3 ⇒ (0, 1, 0)
      case 4 ⇒ (0, 1, 1)
      case 5 ⇒ (1, 1, 1)
      case 6 ⇒ (1, 0, 1)
      case 7 ⇒ (0, 0, 1)
    }

  def ↺(p: Point, % : Int, Σ: Int): Point = {
    import p.{ x, y, z }
    % match {
      case     0 ⇒ (   z,   x,   y )
      case 1 | 2 ⇒ (   y,   z,   x )
      case 3 | 4 ⇒ ( Σ-y, Σ-x,   z )
      case 5 | 6 ⇒ (   y, Σ-z, Σ-x )
      case     7 ⇒ ( Σ-z,   x, Σ-y )
    }
  }

  def apply(n: Int): Point = apply(n, 0)

  import Step._

  private def apply(
        n:   Int       ,
     from: Point       ,
     step:  Step = `0` ,
        Σ:   Int =  1  ,
  ):
    Point =
  {
    val ( d, m ) = n /% N
    val / = d
    val % = m
    val Δ = this.Δ(%)

    val next = Δ * Σ + ↺(from, %, Σ-1)

    if (/ == 0)
      next << step
    else
      apply(
        /,
        next,
        step ++,
        Σ * 2
      )
  }
}
