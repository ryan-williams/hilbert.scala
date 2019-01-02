package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.mix.Size
import com.runsascoded.math.Permutation
import com.runsascoded.utils.Color
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^._

object Page {

  type Sizes = Preview[Size]
   val Sizes = Preview

  type Permutations = Preview[Permutation]
   val Permutations = Preview

  case class State(
           sizes:          Sizes = Sizes(),
    permutations:   Permutations = Permutations(),
           color:          Color = Color(0, 0, 0)
  )

  class Backend($: BackendScope[Size, State]) {

    def mod(fn: State ⇒ State): Callback = $.modState(fn)

    def render(props: Size, s: State): VdomElement = {
      val State(sizes, permutations, color) = s
      implicit val size @ Size(n, n2, n3, _) = sizes.t.getOrElse(props)
      implicit val permutation = permutations.t.getOrElse(Permutation(0, 1, 2))

      div(
        Image.component((
//        Canvas.component((
          size,
          permutation,
          color ⇒ $.modState(_.copy(color = color))
        )),
        Panel.component((
          sizes, size,
          permutations, permutation,
          color,
          mod,
        ))
      )
    }
  }

  val component =
    ScalaComponent
      .builder[Size]("Picker")
      .initialState(State())
      .renderBackend[Backend]
      .build
}
