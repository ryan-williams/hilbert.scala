package com.runsascoded.hilbert

import japgolly.scalajs.react.Callback
import org.scalajs.dom.{ CanvasRenderingContext2D, ImageData, html }

package object components {
  implicit def liftOpt[T](t: T): Option[T] = Some(t)

  type Mod[S] = (S ⇒ S) ⇒ Callback

  case class Canvas(
    elem: html.Canvas,
    ctx: CanvasRenderingContext2D,
    w: Int,
    h: Int,
    img: ImageData
  )

  type Imgs = Map[Drawn, ImageData]
  case class CanvasState(
    canvas: Option[Canvas] = None,
    drawn: Option[Drawn] = None,
    imgs: Imgs = Map()
  )
}
