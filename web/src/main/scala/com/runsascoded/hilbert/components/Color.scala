package com.runsascoded.hilbert.components

import com.runsascoded.math.Permutation

case class Color(r: Int, g: Int, b: Int) {
  def apply(permutation: Permutation): Color = {
    val map = permutation.map
    val elems = Vector(r, g, b)
    Color(
      elems(map(0)),
      elems(map(1)),
      elems(map(2)),
    )
  }
}
