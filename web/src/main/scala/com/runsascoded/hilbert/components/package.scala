package com.runsascoded.hilbert

package object components {
  implicit def liftOpt[T](t: T): Option[T] = Some(t)
}
