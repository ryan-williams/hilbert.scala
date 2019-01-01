package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.mix.Size
import com.runsascoded.math.Permutation

case class Drawn(size: Size, permutation: Permutation)
object Drawn {
  implicit def wrap(implicit size: Size, permutation: Permutation): Drawn = Drawn(size, permutation)
}
