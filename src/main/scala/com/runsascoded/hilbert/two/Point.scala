package com.runsascoded.hilbert.two

case class Point(
  x: Int,
  y: Int
) {
  override def toString: String = s"($x,$y)"
}
