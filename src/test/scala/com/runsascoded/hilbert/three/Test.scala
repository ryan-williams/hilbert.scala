package com.runsascoded.hilbert.three

import hilbert._

class Test
  extends hammerlab.Suite {

  val expecteds =
    Seq(
      P(0, 0, 0),
      P(1, 0, 0),
      P(1, 1, 0),
      P(0, 1, 0),
      P(0, 1, 1),
      P(1, 1, 1),
      P(1, 0, 1),
      P(0, 0, 1),

      P(0, 0, 2),
      P(0, 1, 2),
      P(0, 1, 3),
      P(0, 0, 3),
      P(1, 0, 3),
      P(1, 1, 3),
      P(1, 1, 2),
      P(1, 0, 2),

      P(2, 0, 2),
      P(2, 1, 2),
      P(2, 1, 3),
      P(2, 0, 3),
      P(3, 0, 3),
      P(3, 1, 3),
      P(3, 1, 2),
      P(3, 0, 2),

      P(3, 0, 1),
      P(2, 0, 1),
      P(2, 0, 0),
      P(3, 0, 0),
      P(3, 1, 0),
      P(2, 1, 0),
      P(2, 1, 1),
      P(3, 1, 1),

      P(3, 2, 1),
      P(2, 2, 1),
      P(2, 2, 0),
      P(3, 2, 0),
      P(3, 3, 0),
      P(2, 3, 0),
      P(2, 3, 1),
      P(3, 3, 1),

      P(3, 3, 2),
      P(3, 2, 2),
      P(3, 2, 3),
      P(3, 3, 3),
      P(2, 3, 3),
      P(2, 2, 3),
      P(2, 2, 2),
      P(2, 3, 2),

      P(1, 3, 2),
      P(1, 2, 2),
      P(1, 2, 3),
      P(1, 3, 3),
      P(0, 3, 3),
      P(0, 2, 3),
      P(0, 2, 2),
      P(0, 3, 2),

      P(0, 3, 1),
      P(1, 3, 1),
      P(1, 2, 1),
      P(0, 2, 1),
      P(0, 2, 0),
      P(1, 2, 0),
      P(1, 3, 0),
      P(0, 3, 0),

      P(0, 4, 0),
      P(0, 5, 0),
      P(0, 5, 1),
      P(0, 4, 1),
      P(1, 4, 1),
      P(1, 5, 1),
      P(1, 5, 0),
      P(1, 4, 0),
    )
    .zipWithIndex
    .map(_.swap)

  for {
    (i, p) ← expecteds
  } {
    test(s"to($i)") {
      val actual = `3`(i)
      withClue(s"$actual wasn't $p") {
        ==(actual, p)
      }
    }

    test(s"from$p") {
      val actual = `3`(p)
      withClue(s"$actual wasn't $i") {
        ==(actual, i)
      }
    }
  }

  test("coverage") {
    val N = 1 << 6
    ==(
      (
        for { i ← 0 until N*N*N } yield {
          `3`(i)
        }
      )
      .toSet,
      (
        for {
          x ← 0 until N
          y ← 0 until N
          z ← 0 until N
        } yield
          Point(x, y, z)
      )
      .toSet
    )
  }

  test("diffs") {
    val N = 1 << 6
    (1 to N*N*N).foldLeft(`3`(0)) {
      (prev, i) ⇒
        val next = `3`(i)
        val diff = (next - prev).abs.seq.sorted
        withClue(s"$i ($next - $prev → $diff): ") { ==(diff, Vector(0, 0, 1)) }
        next
    }
  }
}
