package com.runsascoded.hilbert

import com.runsascoded.hilbert

trait Step[Step <: hilbert.Step[Step]] {
  def ++ : Step
}
