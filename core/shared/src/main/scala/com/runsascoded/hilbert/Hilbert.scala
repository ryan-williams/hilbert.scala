package com.runsascoded.hilbert

import com.runsascoded.utils.{ GenInts, Ints }
import hammerlab.iterator._
import hammerlab.shapeless._
import runsascoded.math._

abstract class Hilbert[
  Point
](
  val n: Int
)(
   implicit
   ints:    Ints[Point],
    gen: GenInts[Point],
)
extends Ints.syntax
{
  val N: Int = pow(2)

  // Define basic block on {0,1}^N; indices are built by xor'ing adjacent dimensions' coordinates and summing the result
  val ↷ : Map[Int, Point] =
    gen(2)
      .map {
        p ⇒
          val (i, _) =
            p
              .seq
              .foldRight(
                (
                  0,  // add to this result, bit by bit starting from most-significat ("left") bit
                  0   // last bit seen
                )
              ) {
                case (
                  cur,
                  (res, last)
                ) ⇒
                  val shifted = res << 1
                  val next = last ^ cur
                  (
                    shifted + next,
                    next
                  )
              }

          i → p
      }
      .toMap

  // Reversed map
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
