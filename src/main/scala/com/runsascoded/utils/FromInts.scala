package com.runsascoded.utils

trait FromInts[Out] {
  def apply(ints: Seq[Int]): Out
}
