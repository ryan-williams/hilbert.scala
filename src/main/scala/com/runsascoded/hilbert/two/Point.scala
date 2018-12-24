package com.runsascoded.hilbert.two

case class Point(x: Int, y: Int) {
  def /(n: Int) = Point(x / n, y / n)
  def %(n: Int) = Point(x % n, y % n)
  def /%(n: Int) = (
    this / n,
    this % n
  )
}
object Point {
  implicit def apply(t: (Int, Int)): Point = Point(t._1, t._2)
  implicit def apply(n: Int): Point = Point(n, n)
}
