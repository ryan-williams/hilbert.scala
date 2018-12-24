package com.runsascoded.hilbert.three

import com.runsascoded.hilbert

sealed abstract class Step(override val toString: String)
  extends hilbert.Step[Step]

object Step {
  implicit
  object `0` extends Step("xyz") { def ++ = `1` }
  object `1` extends Step("zxy") { def ++ = `2` }
  object `2` extends Step("yzx") { def ++ = `0` }
  def apply(n: Int): Step =
    n % 3 match {
      case 0 ⇒ `0`
      case 1 ⇒ `1`
      case 2 ⇒ `2`
    }
}
