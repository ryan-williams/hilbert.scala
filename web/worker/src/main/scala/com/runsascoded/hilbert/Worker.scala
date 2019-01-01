package com.runsascoded.hilbert

import boopickle.Default._
import com.runsascoded.hilbert.components.{ Drawn, Img }
import com.runsascoded.worker
import org.scalajs.dom.ImageData

object Worker
extends worker.Main[
  (ImageData, Int, Int, Drawn),
  (Drawn, ImageData)
] {
  def handle(t: (ImageData, Int, Int, Drawn)): Unit = {
    implicit val (img, w, h, drawn @ Drawn(size, permutation)) = t
    Img(img, w, h)
    post(drawn → img)
  }
}
