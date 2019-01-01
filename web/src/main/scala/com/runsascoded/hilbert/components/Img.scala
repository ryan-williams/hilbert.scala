package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.mix.color
import com.runsascoded.hilbert.mix.Size
import com.runsascoded.math.Permutation
import com.runsascoded.utils.Color
import org.scalajs.dom.{ CanvasRenderingContext2D, ImageData }

object Img {
  def apply(
    ctx: CanvasRenderingContext2D,
    w: Int,
    h: Int
  )(
    implicit
    size: Size,
    permutation: Permutation
  ): ImageData = {
    val img = ctx.createImageData(w, h)
    apply(img, w, h)
  }

  def apply(
    img: ImageData,
    w: Int,
    h: Int
  )(
    implicit
    size: Size,
    permutation: Permutation
  ): ImageData = {
    val data = img.data
    val Size(n, n2, n3, _) = size
    for {
      r ← 0 until n3
      c ← 0 until n3
      Color(red, green, blue) = color(c, r, n, n2)
      (x1, x2) = (c * w / n3, (c+1) * w / n3)
      (y1, y2) = (r * h / n3, (r+1) * h / n3)
      x ← x1 until x2
      y ← y1 until y2
      idx = 4 * (w * y + x)
    } {
      data(idx    ) = red
      data(idx + 1) = green
      data(idx + 2) = blue
      data(idx + 3) = 255
    }

    img
  }
}
