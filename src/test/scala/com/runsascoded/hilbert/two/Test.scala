package com.runsascoded.hilbert.two

import hilbert._
import com.runsascoded.hilbert

class Test
extends hilbert.Test(
  `2`,
  Seq(
    P(0, 0),  // 0
    P(1, 0),
    P(1, 1),
    P(0, 1),

    P(0, 2),  // 4
    P(0, 3),
    P(1, 3),
    P(1, 2),

    P(2, 2),  // 8
    P(2, 3),
    P(3, 3),
    P(3, 2),

    P(3, 1),  // 12
    P(2, 1),
    P(2, 0),
    P(3, 0),

    P(4, 0),  // 16
    P(4, 1),
    P(5, 1),
    P(5, 0),

    P(6, 0),  // 20
    P(7, 0),
    P(7, 1),
    P(6, 1),

    P(6, 2),  // 24
    P(7, 2),
    P(7, 3),
    P(6, 3),

    P(5, 3),  // 28
    P(5, 2),
    P(4, 2),
    P(4, 3),
  )
)
