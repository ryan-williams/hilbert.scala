package com.runsascoded.hilbert.three

import Step._

case class Point(x: Int, y: Int, z: Int) {
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
  def  < = Point(y, z, x)

  override def toString: String = s"($x,$y,$z)"
}
object Point {
  implicit def apply(t: (Int, Int, Int)): Point = Point(t._1, t._2, t._3)
  implicit def fromInt(n: Int): Point = Point(n, n, n)
}

sealed abstract class Step(override val toString: String) {
  def next: Step
  def ++ = next
}
object Step {
  object `0` extends Step("xyz") { def next = `1` }
  object `1` extends Step("zxy") { def next = `2` }
  object `2` extends Step("yzx") { def next = `0` }
  def apply(n: Int): Step =
    n % 3 match {
      case 0 ⇒ `0`
      case 1 ⇒ `1`
      case 2 ⇒ `2`
    }
}
