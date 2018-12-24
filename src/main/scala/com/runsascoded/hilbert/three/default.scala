package com.runsascoded.hilbert.three

import com.runsascoded.hilbert.Hilbert

object default
extends Hilbert[Point, Step](3)
{
  def step(n: Int): Step = Step(n)

  def ⟳(p: Point, % : Int, Σ: Int): Point = {
    import p.{ x, y, z }
    % match {
      case     0 ⇒ P(   z,   x,   y )
      case 1 | 2 ⇒ P(   y,   z,   x )
      case 3 | 4 ⇒ P( Σ-y, Σ-x,   z )
      case 5 | 6 ⇒ P(   y, Σ-z, Σ-x )
      case     7 ⇒ P( Σ-z,   x, Σ-y )
    }
  }

  // TODO: formalize that these are inverses of one another
  def ⟲(p: Point, Δ: Int, Σ: Int): Point = {
    import p.{ x, y, z }
    Δ match {
      case     0 ⇒ P(   y,   z,   x )
      case 1 | 2 ⇒ P(   z,   x,   y )
      case 3 | 4 ⇒ P( Σ-y, Σ-x,   z )
      case 5 | 6 ⇒ P( Σ-z,   x, Σ-y )
      case     7 ⇒ P(   y, Σ-z, Σ-x )
    }
  }
}
