package com.runsascoded.hilbert.three

import Step._

case class Point(x: Int, y: Int, z: Int) {
  def +(p: Point) = Point(x + p.x, y + p.y, z + p.z)
  def *(p: Point) = Point(x * p.x, y * p.y, z * p.z)
  def /(p: Point) = Point(x / p.x, y / p.y, z / p.z)
  def %(p: Point) = Point(x % p.x, y % p.y, z % p.z)

  def /%(n: Point) = (
    this / n,
    this % n
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

  override def toString: String = s"($x,$y,$z)"
}
object Point {
  implicit def apply(t: (Int, Int, Int)): Point = Point(t._1, t._2, t._3)
  implicit def fromInt(n: Int): Point = Point(n, n, n)
}

sealed abstract class Step(val next: Step, override val toString: String) {
  def ++ = next
}
object Step {
  object `0` extends Step(`1`, "xyz")
  object `1` extends Step(`2`, "zxy")
  object `2` extends Step(`0`, "yzx")
}
