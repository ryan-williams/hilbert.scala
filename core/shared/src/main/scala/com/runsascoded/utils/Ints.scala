package com.runsascoded.utils

import shapeless._
import nat._
import shapeless.ops.hlist._

trait Ints[T] {
  val n: Int
  def +(l: T, r: T): T
  def -(l: T, r: T): T
  def *(l: T, r: T): T
  def /(l: T, r: T): T
  def %(l: T, r: T): T
  def /%(l: T, r: T): (T, T) =
    (
      /(l, r),
      %(l, r)
    )

  def max(l: T): Int = seq(l).max
  def seq(l: T): List[Int]

  def int(n: Int): T

  def <<(l: T, n: Int = 1) : T
  def >>(l: T, n: Int = 1) : T
}

object Ints {
  implicit val hnil: Ints[HNil] =
    new Ints[HNil] {
      val n = 0
      def   +(l: HNil, r: HNil) = HNil
      def   -(l: HNil, r: HNil) = HNil
      def   *(l: HNil, r: HNil) = HNil
      def   /(l: HNil, r: HNil) = HNil
      def   %(l: HNil, r: HNil) = HNil
      def seq(l: HNil         ) =  Nil
      def int(n: Int          ) = HNil
      def <<(l: HNil, n: Int) = HNil
      def >>(l: HNil, n: Int) = HNil
    }

  implicit def cons[L <: HList](
    implicit
    tail: Ints[L],
    rl: RotateLeft .Aux[Int :: L, _1, Int :: L],
    rr: RotateRight.Aux[Int :: L, _1, Int :: L],
  )
  : Ints[Int :: L]
  =
    new Ints[Int :: L] {
      val n = tail.n + 1
      type T = Int :: L
      def   +(l: T, r: T): T = (l.head + r.head) :: tail.+(l.tail, r.tail)
      def   -(l: T, r: T): T = (l.head - r.head) :: tail.-(l.tail, r.tail)
      def   *(l: T, r: T): T = (l.head * r.head) :: tail.*(l.tail, r.tail)
      def   /(l: T, r: T): T = (l.head / r.head) :: tail./(l.tail, r.tail)
      def   %(l: T, r: T): T = (l.head % r.head) :: tail.%(l.tail, r.tail)
      def seq(l: T): List[Int] = scala.::(l.head, tail.seq(l.tail))
      def int(n: Int): T = n :: tail.int(n)

      def <<(l: T, n: Int): T = if (n > this.n) <<(l, n % this.n) else if (n > this.n - n) >>(l, this.n - n) else if (n == 0) l else <<(l.rotateLeft (1), n - 1)
      def >>(l: T, n: Int): T = if (n > this.n) >>(l, n % this.n) else if (n > this.n - n) <<(l, this.n - n) else if (n == 0) l else <<(l.rotateRight(1), n - 1)
    }

  implicit def cc[T, L <: HList](
    implicit
    g: Generic.Aux[T, L],
    ints: Ints[L]
  )
  : Ints[T]
  =
    new Ints[T] {
      val n = ints.n
      def   +(l: T, r: T): T = g.from(ints.+(g.to(l), g.to(r)))
      def   -(l: T, r: T): T = g.from(ints.-(g.to(l), g.to(r)))
      def   *(l: T, r: T): T = g.from(ints.*(g.to(l), g.to(r)))
      def   /(l: T, r: T): T = g.from(ints./(g.to(l), g.to(r)))
      def   %(l: T, r: T): T = g.from(ints.%(g.to(l), g.to(r)))
      def seq(l: T): List[Int] = ints.seq(g.to(l))
      def int(n: Int): T = g.from(ints.int(n))
      def <<(l: T, n: Int) : T = g.from(ints.<<(g.to(l), n))
      def >>(l: T, n: Int) : T = g.from(ints.>>(g.to(l), n))
    }

  implicit class Ops[T](val l: T) extends AnyVal {
    @inline def   +(r: T)(implicit i: Ints[T]):        T  = i   + (l, r)
    @inline def   -(r: T)(implicit i: Ints[T]):        T  = i   - (l, r)
    @inline def   *(r: T)(implicit i: Ints[T]):        T  = i   * (l, r)
    @inline def   /(r: T)(implicit i: Ints[T]):        T  = i   / (l, r)
    @inline def   %(r: T)(implicit i: Ints[T]):        T  = i   % (l, r)
    @inline def  /%(r: T)(implicit i: Ints[T]):    (T, T) = i   /%(l, r)

    @inline def  <<(n: Int = 1)(implicit i: Ints[T]): T = i <<(l, n)
    @inline def  >>(n: Int = 1)(implicit i: Ints[T]): T = i >>(l, n)
    @inline def seq      (implicit i: Ints[T]): List[Int] = i seq  l
    @inline def max      (implicit i: Ints[T]):      Int  = i seq  l max
  }

  trait syntax {
    @inline implicit def intsOps[T: Ints](t: T): Ops[T] = Ops(t)
    @inline implicit def intintsOps[T](n: Int)(implicit i: Ints[T]): T = i.int(n)
  }
}
