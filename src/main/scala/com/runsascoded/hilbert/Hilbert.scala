package com.runsascoded.hilbert

import com.runsascoded.utils.{ FromInts, Ints }
import hammerlab.iterator._
import hammerlab.shapeless._
import runsascoded.math._

abstract class Hilbert[
  Point
](
  val n: Int
)(
   implicit
    ints: Ints[Point],
   point: FromInts[Point]
)
extends Ints.syntax
{
  val N: Int = pow(2)
  val ↷ : Map[Int, Point] =
    (0 until N)
      .map {
        i ⇒
          i →
            point(
              (0 to n)
                .map {
                  b ⇒ (i & (1 << b)) >> b
                }
                .sliding2
                .map {
                  case (cur, next) ⇒ cur ^ next
                }
                .toVector
            )
      }
      .toMap

  val ↶ : Map[Point, Int] =
    for {
      (k, v) ← ↷
    } yield
      v → k

  // TODO: formalize that these are inverses of one another
  def ⟲(p: Point, % : Int, Σ: Int): Point
  def ⟳(p: Point, % : Int, Σ: Int): Point

  def pow(Σ: Int, n: Int = this.n): Int =
    if (n == 0)
      1
    else
      Σ * pow(Σ, n - 1)

  def apply(n: Int): Point = apply(n, 0)
  private def apply(
       n:   Int     ,
    from: Point     ,
    step:   Int = 0 ,
       Σ:   Int = 1 ,
  ):
    Point =
  {
    val ( d, m ) = toDivMod(n) /% N
    val / = d
    val % = m
    val Δ = ↷(%)

    val next = Δ * Σ + ⟳(from, %, Σ-1)

    if (/ == 0)
      next << step
    else
      apply(
        /,
        next,
        step + 1,
        Σ * 2
      )
  }

  def apply(p: Point): Int = {
    val (pow, n) = p.max.log(2)
    apply(p >> n, pow, n)
  }

  private def apply(p: Point, Σ: Int, n: Int): Int = {
    if (n < 0)
      0
    else {
      val (top, rest) = p /% ints.int(Σ)
      val Δ = ↶(top)
      val next = ⟲(rest, Δ, Σ-1)
      pow(Σ) * Δ + apply(next, Σ / 2, n - 1)
    }
  }
}
