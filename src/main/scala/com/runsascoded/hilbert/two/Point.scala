package com.runsascoded.hilbert.two

import com.runsascoded.utils.FromInts

case class Point(
  x: Int,
  y: Int
) {
  override def toString: String = s"($x,$y)"
}

object Point {
  implicit val fromInts: FromInts[Point] = { case Seq(x, y) ⇒ Point(x, y) }
}
