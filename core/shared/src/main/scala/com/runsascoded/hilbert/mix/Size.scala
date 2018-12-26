package com.runsascoded.hilbert.mix

/**
 * Enumeration of the possible sizes of a hilbert-curve projection of a cube to a square
 *
 * - every cube and square length must be a power of 2
 * - the cube must have the same number of total elements as the square
 *
 * This implies that the total number of elements must be a power of 64 (2⁶), leaving just 4 sensible sizes for a cube
 * where each dimension is an 8-bit color intensity:
 *
 * - 2⁶: 4x4x4 cube, 8x8 square
 * - 2¹²: 16³ cube, 64² square
 * - 2¹⁸: 64³ cube, 512² square
 * - 2²⁴: 256³ cube, 4096² square
 *
 * We number these 1 through 4 here, and store the corresponding cube's (`n2`) and square's (`n3`) edge-lengths
 *
 * @param n 1, 2, 3, or 4 (corresponding to 64**n colors displayed; 64**4 == 2**24 is the full 3-byte color-space
 */
sealed abstract class Size(
  val n: Int
) {
  val n2 = 1 << (2*n)  // virtual RGB-cube edge-length
  val n3 = 1 << (3*n)  // color-squares per side
  val n6 = 1 << (6*n)  // total number of color-squares
}
object Size {
  object `1` extends Size(1)
  object `2` extends Size(2)
  object `3` extends Size(3)
  object `4` extends Size(4)
  def unapply(size: Size): Option[(Int, Int, Int, Int)] = {
    import size._
    Some(
      (
        n,
        n2,
        n3,
        n6
      )
    )
  }
}
