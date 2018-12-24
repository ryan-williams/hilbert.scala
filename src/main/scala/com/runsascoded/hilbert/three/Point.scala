package com.runsascoded.hilbert.three

import com.runsascoded.hilbert
import Step._
import com.runsascoded.hilbert.{ FromInt, FromInts }

case class Point(x: Int, y: Int, z: Int)
  extends hilbert.Point[Step, Point] {
  def +(p: Point) = Point(x + p.x, y + p.y, z + p.z)
  def -(p: Point) = Point(x - p.x, y - p.y, z - p.z)
  def *(p: Point) = Point(x * p.x, y * p.y, z * p.z)
  def /(p: Point) = Point(x / p.x, y / p.y, z / p.z)
  def %(p: Point) = Point(x % p.x, y % p.y, z % p.z)

  import math.{ abs => | }
  def abs = Point(|(x), |(y), |(z))
  def seq = Vector(x, y, z)
  def max = {
    import math.{ max ⇒ m }
    m(m(x, y), z)
  }

  def /%(p: Point) = (
    this / p,
    this % p
  )

  def >>(step: Step): Point =
    step match {
      case `0` ⇒ this
      case `1` ⇒ >
      case `2` ⇒ >>
    }

  def <<(step: Step): Point =
    step match {
      case `0` ⇒ this
      case `1` ⇒ >>
      case `2` ⇒ >
    }

  def  > = Point(z, x, y)
  def >> = Point(y, z, x)
  def  < = Point(y, z, x)

  override def toString: String = s"($x,$y,$z)"
}
object Point {
  implicit val fromInt : FromInt [Point] = {                n  ⇒ Point(n, n, n) }
  implicit val fromInts: FromInts[Point] = { case Seq(x, y, z) ⇒ Point(x, y, z) }
}

sealed abstract class Step(override val toString: String)
  extends hilbert.Step[Step]

object Step {
  implicit
  object `0` extends Step("xyz") { def ++ = `1` }
  object `1` extends Step("zxy") { def ++ = `2` }
  object `2` extends Step("yzx") { def ++ = `0` }
  def apply(n: Int): Step =
    n % 3 match {
      case 0 ⇒ `0`
      case 1 ⇒ `1`
      case 2 ⇒ `2`
    }
}
