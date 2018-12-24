package com.runsascoded.utils

trait FromInt[Out] {
  def apply(int: Int): Out
}
