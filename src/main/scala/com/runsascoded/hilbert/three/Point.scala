package com.runsascoded.hilbert.three

import com.runsascoded.hilbert
import Step._
import com.runsascoded.utils.{ FromInt, FromInts }

case class Point(
  x: Int,
  y: Int,
  z: Int
)
extends hilbert.Point[Step, Point]
{
  def +(p: Point) = Point(x + p.x, y + p.y, z + p.z)
  def -(p: Point) = Point(x - p.x, y - p.y, z - p.z)
  def *(p: Point) = Point(x * p.x, y * p.y, z * p.z)
  def /(p: Point) = Point(x / p.x, y / p.y, z / p.z)
  def %(p: Point) = Point(x % p.x, y % p.y, z % p.z)

  def seq = Vector(x, y, z)

  @inline def >>(step: Step): Point =
    step match {
      case `0` ⇒ this
      case `1` ⇒ >
      case `2` ⇒ >>
    }

  @inline def <<(step: Step): Point =
    step match {
      case `0` ⇒ this
      case `1` ⇒ >>
      case `2` ⇒ >
    }

  def  > = Point(z, x, y)
  def >> = Point(y, z, x)

  override def toString: String = s"($x,$y,$z)"
}

object Point {
  implicit val fromInt : FromInt [Point] = {                n  ⇒ Point(n, n, n) }
  implicit val fromInts: FromInts[Point] = { case Seq(x, y, z) ⇒ Point(x, y, z) }
}

