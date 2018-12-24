package com.runsascoded.hilbert

import com.runsascoded.hilbert
import org.hammerlab.cmp.Cmp
import math.abs

abstract class Test[
  Point <: hilbert.Point[Step, Point],
   Step <: hilbert. Step[Step       ],
      D
](
  h: Hilbert[Point, Step],
  expecteds: Seq[Point],
  N: Int = 1 << 6
)(
  implicit
  cmp: Cmp.Aux[Point, D],
  point: FromInts[Point]
)
extends hammerlab.Suite
{
  for {
    (i, p) ← expecteds.zipWithIndex.map(_.swap)
  } {
    test(s"to($i)") {
      val actual = h(i)
      withClue(s"$actual wasn't $p") {
        ==(actual, p)
      }
    }

    test(s"from$p") {
      val actual = h(p)
      withClue(s"$actual wasn't $i") {
        ==(actual, i)
      }
    }
  }

  def seqs(N: Int, n: Int = h.n): Seq[List[Int]] =
    if (n == 0)
      Seq(Nil)
    else
      for {
        head ← 0 until N
        tail ← seqs(N, n - 1)
      } yield
        head :: tail

  import h.{ pow, n }

  test("coverage") {
    ==(
      (
        for { i ← 0 until pow(N) } yield {
          h(i)
        }
      )
      .toSet,
      seqs(N)
        .map(point(_))
        .toSet
    )
  }

  test("diffs") {
    val expected = Vector.fill(n - 1)(0) ++ Vector(1)
    (1 to pow(N))
      .foldLeft(h(0)) {
        (prev, i) ⇒
          val next = h(i)
          val diff =
            (next - prev)
              .seq
              .map(abs)
              .sorted

          withClue(s"$i ($next - $prev → $diff): ") {
            ==(diff, expected)
          }

          next
      }
  }
}
