package com.runsascoded.hilbert.css

import scalacss.DevDefaults._

object Style
  extends StyleSheet.Inline {
  import dsl._

  val pre =
    style(
      margin(0.2 em, 0 px)
    )

  val color =
    style(
      fontFamily := "monospace",
      fontSize(1.5 em)
    )

  val thumb =
    style(
       width(80 px),
      height(80 px),
    )

  val panel =
    style(
      float.left,
      paddingLeft(20 px)
    )

  val buttons =
    style(
      paddingTop(10 px)
    )

  val sizeButton =
    style(
      marginRight(5 px)
    )

  val canvas =
    style(
      float.left,
       width(1280 px),
      height(1280 px),
    )
}
