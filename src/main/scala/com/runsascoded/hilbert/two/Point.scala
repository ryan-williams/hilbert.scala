package com.runsascoded.hilbert.two

import com.runsascoded.hilbert
import com.runsascoded.hilbert.{ FromInt, FromInts }

sealed abstract class Step(override val toString: String)
  extends hilbert.Step[Step]

object Step {
  implicit
  object `0` extends Step("xy") { def ++ = `1` }
  object `1` extends Step("yx") { def ++ = `0` }
}

case class Point(x: Int, y: Int)
  extends hilbert.Point[Step, Point] {

  override def +(p: Point): Point = Point(x + p.x, y + p.y)
  override def -(p: Point): Point = Point(x - p.x, y - p.y)
  override def *(p: Point): Point = Point(x * p.x, y * p.y)
  override def /(p: Point): Point = Point(x / p.x, y / p.y)
  override def %(p: Point): Point = Point(x % p.x, y % p.y)

  override def max: Int = math.max(x, y)

  import Step._

  override def <<(step: Step): Point =
    step match {
      case `0` ⇒ this
      case `1` ⇒ Point(y, x)
    }

  @inline override def >>(step: Step): Point = <<(step)
}

object Point {
  implicit val fromInt : FromInt [Point] = {             n  ⇒ Point(n, n) }
  implicit val fromInts: FromInts[Point] = { case Seq(x, y) ⇒ Point(x, y) }
}
