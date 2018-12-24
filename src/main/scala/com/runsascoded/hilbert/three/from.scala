package com.runsascoded.hilbert.three

//import runsascoded.math._

//trait from {
//  def apply(p: Point): Int = {
//    val (pow, n) = p.max.log(2)
//    val step = Step(n)
//    apply(p >> step, pow, n)
//  }
//
//  def Δ(p: Point): Int =
//    p match {
//      case Point(0, 0, 0) ⇒ 0
//      case Point(1, 0, 0) ⇒ 1
//      case Point(1, 1, 0) ⇒ 2
//      case Point(0, 1, 0) ⇒ 3
//      case Point(0, 1, 1) ⇒ 4
//      case Point(1, 1, 1) ⇒ 5
//      case Point(1, 0, 1) ⇒ 6
//      case Point(0, 0, 1) ⇒ 7
//    }
//
//  def ⟲(p: Point, Δ: Int, Σ: Int): Point = {
//    import p.{ x, y, z }
//    Δ match {
//      case     0 ⇒ (   y,   z,   x )
//      case 1 | 2 ⇒ (   z,   x,   y )
//      case 3 | 4 ⇒ ( Σ-y, Σ-x,   z )
//      case 5 | 6 ⇒ ( Σ-z,   x, Σ-y )
//      case     7 ⇒ (   y, Σ-z, Σ-x )
//    }
//  }
//
//  private def apply(p: Point, Σ: Int, n: Int): Int = {
//    if (n < 0)
//      0
//    else {
//      val (top, rest) = p /% Σ
//      val Δ = this.Δ(top)
//      val next = ⟲(rest, Δ, Σ-1)
//      Σ * Σ * Σ * Δ + apply(next, Σ / 2, n - 1)
//    }
//  }
//}
