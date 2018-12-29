package com.runsascoded.hilbert

import com.runsascoded.math.Permutation
import com.runsascoded.utils.{ Color, Ints }
import hilbert._

package object mix
  extends Ints.syntax
{
  def color(c: Int, r: Int, n: Int, n2: Int)(implicit permutation: Permutation): Color = {
    val three.P(_r, _g, _b) =
      `3`(
        `2`(
          // top level should go right, down, left; this is true without a shift for n == 1 (in an 8x8 square, the top
          // level is 2x2, which is two power-of-two flips from the bottom level), and each subsequent `n` adds 3
          // powers of 2, requiring toggling the orientation
          two.P(c, r) >> (n - 1)
        )
        // Keep the highest level constant: red is the least significant axis, then green, then blue.
        // When n == 1 (4x4x4 cube), the top-level dimension is one from the RGB bottom level, namely GBR, so we shift
        // by 1.
        // Subsequent cases multiply the cube's edge-length by 4, requiring two additional shifts.
      ) >> (2*n - 1)
    Color(
      _r * 255 / (n2 - 1),
      _g * 255 / (n2 - 1),
      _b * 255 / (n2 - 1)
    )(
      permutation
    )
  }
}
