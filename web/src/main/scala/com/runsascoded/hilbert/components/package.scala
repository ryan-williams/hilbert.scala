package com.runsascoded.hilbert

import japgolly.scalajs.react.Callback
import org.scalajs.dom.{ CanvasRenderingContext2D, ImageData, html }

package object components {
  implicit def liftOpt[T](t: T): Option[T] = Some(t)

  type Mod[S] = (S ⇒ S) ⇒ Callback
}
