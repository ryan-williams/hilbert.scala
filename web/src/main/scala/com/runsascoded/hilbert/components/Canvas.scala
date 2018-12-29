package com.runsascoded.hilbert.components

import org.scalajs.dom.{ CanvasRenderingContext2D, html }

case class Canvas(
  elem: html.Canvas,
  ctx: CanvasRenderingContext2D,
  w: Int,
  h: Int
)
