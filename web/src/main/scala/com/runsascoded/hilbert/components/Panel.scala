package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.components.Page.{ Permutations, Sizes, State }
import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix.Size
import com.runsascoded.hilbert.mix.Size._
import com.runsascoded.math.Permutation
import com.runsascoded.utils.Color
import japgolly.scalajs.react.{ Callback, ScalaComponent }
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.html_<^.<._
import scalacss.ScalaCssReact._

object Panel {

  val component =
    ScalaComponent
      .builder[
        (
          Sizes,
          Size,
          Permutations,
          Permutation,
          Option[Color],
          Mod[State],
        )
      ](
        "Panel"
      )
      .render_P {
        case (sizes, size, permutations, permutation, color, modState) ⇒
          div(
            Style.panel,
            color
            .map {
              color ⇒
                div(
                  Style.color,
                  div(
                    Style.thumb,
                    ^.backgroundColor := color.rgb,
                  ),
                  pre(Style.pre, color.rgb),
                  pre(Style.pre, color.hex)
                )
            },
            {
              def button(sz: Size, label: String) =
                Button[Size](
                  Button.Props(
                    sz,
                    size == sz,
                    sizes.click.contains(sz),
                    label,
                    fn ⇒ modState(_.copy(sizes = fn(sizes)))
                  )
                )

              div(
                Style.buttons,
                button(`1`, "8x8"),
                button(`2`, "64x64"),
                button(`3`, "512x512"),
              )
            },
            {
              def button(p: Permutation, label: String) =
                Button[Permutation](
                  Button.Props(
                    p,
                    p == permutation,
                    permutations.click.contains(p),
                    label,
                    fn ⇒ modState(_.copy(permutations = fn(permutations)))
                  )
                )

              div(
                Style.buttons,
                button(Permutation(0, 1, 2), "rgb"),
                button(Permutation(0, 2, 1), "rbg"),
                button(Permutation(1, 0, 2), "grb"),
                button(Permutation(1, 2, 0), "gbr"),
                button(Permutation(2, 0, 1), "brg"),
                button(Permutation(2, 1, 0), "bgr"),
              )
            }
          )
      }
      .build
}
