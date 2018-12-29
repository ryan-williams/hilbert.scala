package com.runsascoded.utils

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
  def rgb: String = rgb(true)
  def rgb(fixed: Boolean): String =
    if (fixed)
      Seq(r, g, b)
        .map(
          "% 3d"
            .format(_)
            .takeRight(3)
        )
        .mkString("rgb(", ",", ")")
    else
      s"rgb($r,$g,$b)"

  def hex: String = hex(true)
  def hex(abbrev: Boolean): String =
  {
    val hexs =
      Seq(r, g, b)
        .map { "%02x".format(_) }

    (
      // shorten to one-char hex-strings if all colors' hex-strings allow it (i.e. are two copies of the
      // same character)
      if (
        abbrev &&
        hexs.forall {
          s ⇒ s.apply(0) == s.apply(1)
        }
      )
        hexs.map(_.drop(1))
      else
        hexs
    )
    .mkString("#", "", "")
  }

}
