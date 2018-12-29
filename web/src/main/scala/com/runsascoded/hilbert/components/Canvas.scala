package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix
import com.runsascoded.hilbert.mix.Size
import com.runsascoded.math.Permutation
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.raw.ImageData
import org.scalajs.dom.{ CanvasRenderingContext2D, html }
import scalacss.ScalaCssReact._

object Canvas {
  case class Drawn(size: Size, permutation: Permutation)
  object Drawn {
    implicit def wrap(implicit size: Size, permutation: Permutation): Drawn = Drawn(size, permutation)
  }

  type Sizes = Preview[Size]
   val Sizes = Preview

  type Permutations = Preview[Permutation]
   val Permutations = Preview

  type Props = (
    Size,
    Permutation,
    Mod[Picker.State]
  )

  case class Canvas(
    elem: html.Canvas,
    ctx: CanvasRenderingContext2D,
    w: Int,
    h: Int
  )

  case class State(
    canvas: Option[Canvas] = None,
    drawn: Option[Drawn] = None,
    imgs: Map[Drawn, ImageData] = Map()
  )

  class Backend($: BackendScope[Props, State]) {

    def render(props: Props, state: State): VdomElement = {
      implicit val (
        size @ Size(n, n2, n3, _),
        permutation,
        modState
      )
      = props

      val State(canvas, _, _) = state

      <.canvas(
        Style.canvas,
        ^.onMouseMove ==> {
          e: ReactMouseEvent ⇒
            canvas.fold { Callback() } {
              case Canvas(canvas, _, w, h) ⇒

                val x = e.clientX - canvas.offsetLeft
                val y = e.clientY - canvas.offsetTop

                val c = x * n3 / w toInt
                val r = y * n3 / h toInt

                val color = mix.color(c, r, n, n2)

                modState(_.copy(color = color))
            }
        }
      )
    }
  }

  val component =
    ScalaComponent
      .builder[Props]("Picker")
      .initialState(State())
      .renderBackend[Backend]
      .componentWillReceiveProps {
        p ⇒
          val (size, permutation, _) = p.nextProps
          val drawn = Drawn(size, permutation)
          Callback()
      }
      .componentDidMount {
        p ⇒
          import p._
          val (_, _, cb) = p.props

          val canvas =
            getDOMNode
              .node
              .asInstanceOf[html.Canvas]

          val w = canvas.clientWidth
          val h = canvas.clientHeight

          canvas.setAttribute( "width", w.toString)
          canvas.setAttribute("height", h.toString)

          val ctx =
            canvas
              .getContext("2d")
              .asInstanceOf[CanvasRenderingContext2D]

          val c =
            Canvas(
              canvas,
              ctx,
              w,
              h
            )

          modState(
            _.copy(
              canvas = c
            )
          )
      }
      .build
}
