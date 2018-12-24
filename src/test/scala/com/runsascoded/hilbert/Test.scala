package com.runsascoded.hilbert

import com.runsascoded.utils.{ GenInts, Ints }
import org.hammerlab.cmp.Cmp

import scala.math.abs

abstract class Test[
  Point,
      D
](
  h: Hilbert[Point],
  expecteds: Seq[Point],
  N: Int = 1 << 6
)(
  implicit
   cmp: Cmp.Aux[Point, D],
  ints: Ints[Point],
   gen: GenInts[Point]
)
extends hammerlab.Suite
   with Ints.syntax
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

  import h.{ n, pow }

  test("coverage") {
    ==(
      (0 until pow(N))
        .map(h(_))
        .toSet,
      gen(N)
        .toSet
    )
  }

  test("diffs") {
    val expected = List.fill(n - 1)(0) ++ List(1)
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
