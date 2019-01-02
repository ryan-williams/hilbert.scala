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
       width(120 px),
      height(120 px),
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

  val pressed = style("pressed")(
    backgroundColor(darkgray)
  )

  val none = style()

  val sizeButton =
    style(
      fontSize(1.5 em),
      padding(0.2 em, 0.4 em),
    )

  val canvas =
    style(
      float.left,
       width(512 px),
      height(512 px),
    )

  val image =
    style(
      float.left
    )
}
