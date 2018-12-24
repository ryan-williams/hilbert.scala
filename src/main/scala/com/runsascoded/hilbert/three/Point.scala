package com.runsascoded.hilbert.three

import com.runsascoded.utils.FromInts

case class Point(
  x: Int,
  y: Int,
  z: Int
) {
  override def toString: String = s"($x,$y,$z)"
}

object Point {
  implicit val fromInts: FromInts[Point] = { case Seq(x, y, z) ⇒ Point(x, y, z) }
}

