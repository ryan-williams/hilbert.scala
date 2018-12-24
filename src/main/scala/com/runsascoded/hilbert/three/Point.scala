package com.runsascoded.hilbert.three

case class Point(
  x: Int,
  y: Int,
  z: Int
) {
  override def toString: String = s"($x,$y,$z)"
}
