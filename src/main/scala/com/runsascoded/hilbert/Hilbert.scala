package com.runsascoded.hilbert

import com.runsascoded.hilbert
import com.runsascoded.utils.{ FromInt, FromInts }
import hammerlab.iterator._
import hammerlab.shapeless._
import runsascoded.math._

abstract class Hilbert[
  Point <: hilbert.Point[Step, Point],
   Step <: hilbert. Step[Step       ]
](
  val n: Int
)(
   implicit
   _steps: InstanceMap[Step],
   point: FromInts[Point],
   fixed: FromInt [Point]
) {

  val steps: Vector[Step] =
    _steps()
      .values
      .toVector
      .sortBy(
        _
          .getClass
          .getSimpleName
      )

  @inline def step(n: Int): Step = steps(n % this.n)

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
       n:   Int            ,
    from: Point            ,
    step:  Step = steps(0) ,
       Σ:   Int =       1  ,
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
        step ++,
        Σ * 2
      )
  }

  def apply(p: Point): Int = {
    val (pow, n) = p.max.log(2)
    val step = this.step(n)
    apply(p >> step, pow, n)
  }

  private def apply(p: Point, Σ: Int, n: Int): Int = {
    if (n < 0)
      0
    else {
      val (top, rest) = p /% Σ
      val Δ = ↶(top)
      val next = ⟲(rest, Δ, Σ-1)
      pow(Σ) * Δ + apply(next, Σ / 2, n - 1)
    }
  }

  @inline implicit def pointFromInt(n: Int): Point = fixed(n)
}
