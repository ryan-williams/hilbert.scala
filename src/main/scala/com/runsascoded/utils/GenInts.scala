package com.runsascoded.utils

import shapeless._

trait GenInts[T] {
  def apply(n: Int): Seq[T]
}
object GenInts {
  implicit val hnil: GenInts[HNil] = n ⇒ Seq(HNil)
  implicit def cons[L <: HList](implicit tail: GenInts[L]): GenInts[Int :: L] =
    {
      n ⇒
        for {
          h ← 0 until n
          t ← tail(n)
        } yield
          h :: t
    }
  implicit def cc[T, L <: HList](
    implicit
    g: Generic.Aux[T, L],
    tail: GenInts[L]
  )
  : GenInts[T] =
    tail(_).map(g.from)
}
