package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.mix.Size
import org.scalajs.dom.{ CanvasRenderingContext2D, ImageData, html }

case class Canvas(
  elem: html.Canvas,
  ctx: CanvasRenderingContext2D,
  w: Int,
  h: Int
)
