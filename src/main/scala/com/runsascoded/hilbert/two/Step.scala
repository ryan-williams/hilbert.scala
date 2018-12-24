package com.runsascoded.hilbert.two

import com.runsascoded.hilbert

sealed abstract class Step(override val toString: String)
  extends hilbert.Step[Step]

object Step {
  implicit
  object `0` extends Step("xy") { def ++ = `1` }
  object `1` extends Step("yx") { def ++ = `0` }
}
