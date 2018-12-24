package com.runsascoded.hilbert.two

import com.runsascoded.hilbert.Hilbert

object default
extends Hilbert[Point, Step](2)
{
  def ⟳(p: Point, % : Int, Σ: Int): Point = {
    import p.{ x, y }
    % match {
      case     0 ⇒ P(   y,   x )
      case 1 | 2 ⇒ P(   x,   y )
      case     3 ⇒ P( Σ-y, Σ-x )
    }
  }

  @inline def ⟲(p: Point, % : Int, Σ: Int): Point = ⟳(p, %, Σ)
}
