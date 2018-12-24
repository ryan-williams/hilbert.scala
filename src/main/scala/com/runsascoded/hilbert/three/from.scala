package com.runsascoded.hilbert.three

trait from {
  def apply(p: Point): Int = apply(p, 0)

  private def apply(p: Point, from: Point): Int = {
    val (rest, mod) = p /% N
    ???
  }
}
