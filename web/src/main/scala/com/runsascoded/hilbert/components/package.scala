package com.runsascoded.hilbert

import japgolly.scalajs.react.Callback

package object components {
  implicit def liftOpt[T](t: T): Option[T] = Some(t)

  type Mod[S] = (S ⇒ S) ⇒ Callback
}
