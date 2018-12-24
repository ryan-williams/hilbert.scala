package com.runsascoded.hilbert.two

import com.runsascoded.hilbert
import com.runsascoded.utils.{ FromInt, FromInts }

case class Point(
  x: Int,
  y: Int
)
extends hilbert.Point[Step, Point]
{
  def +(p: Point): Point = Point(x + p.x, y + p.y)
  def -(p: Point): Point = Point(x - p.x, y - p.y)
  def *(p: Point): Point = Point(x * p.x, y * p.y)
  def /(p: Point): Point = Point(x / p.x, y / p.y)
  def %(p: Point): Point = Point(x % p.x, y % p.y)

  def seq = Vector(x, y)

  import Step._

  def <<(step: Step): Point =
    step match {
      case `0` ⇒ this
      case `1` ⇒ Point(y, x)
    }

  @inline def >>(step: Step): Point = <<(step)

  override def toString: String = s"($x,$y)"
}

object Point {
  implicit val fromInt : FromInt [Point] = {             n  ⇒ Point(n, n) }
  implicit val fromInts: FromInts[Point] = { case Seq(x, y) ⇒ Point(x, y) }
}
