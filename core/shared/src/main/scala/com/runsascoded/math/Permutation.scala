package com.runsascoded.math

case class Permutation(a: Int, b: Int, c: Int) {
  val map = Vector(a, b, c).zipWithIndex.map(_.swap).toMap
  require(map.size == 3, s"Invalid permutation: $a $b $c")
}
