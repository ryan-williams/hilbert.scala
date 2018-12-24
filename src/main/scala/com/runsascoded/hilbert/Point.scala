package com.runsascoded.hilbert

trait Point[Step, P <: Point[Step, P]] {
  def +(p: P): P
  def -(p: P): P
  def *(p: P): P
  def /(p: P): P
  def %(p: P): P

  def /%(p: P): (P, P) = (this / p, this % p)

  def max: Int = seq.max
  def seq: Vector[Int]

  def <<(step: Step): P
  def >>(step: Step): P
}
