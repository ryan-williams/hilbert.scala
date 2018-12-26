package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix.Size
import com.runsascoded.hilbert.{ three, two }
import com.runsascoded.utils.Ints
import hilbert._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.ImageData
import org.scalajs.dom.{ CanvasRenderingContext2D, html }
import scalacss.ScalaCssReact._

object Picker {

//  val size = 800

//  sealed trait Color
//  case class RGB(r: Int, g: Int, b: Int) extends Color
//  case class HSV(h: Int, s: Int, v: Int) extends Color

  case class State(
      size: Option[  Size] = None,
    canvas: Option[Canvas] = None,
     color: Option[ Color] = None
  )

  implicit def liftOpt[T](t: T): Option[T] = Some(t)

  class Backend($: BackendScope[Size, State]) extends Ints.syntax {
    import $.modState

    def color(c: Int, r: Int, n: Int, n2: Int): Color = {
      val three.P(_r, _g, _b) =
        `3`(
          `2`(
            two.P(c, r) >> (n - 1)
          )
        ) >> (2*n - 1)
      Color(
        _r * 255 / (n2 - 1),
        _g * 255 / (n2 - 1),
        _b * 255 / (n2 - 1)
      )
    }

    def draw(canvas: Canvas, sz: Size): Option[ImageData] = {
      val Size(n, n2, n3, _) = sz
      val Canvas(_, imgs, ctx, w, h) = canvas
      imgs
        .get(sz)
        .fold[Option[ImageData]] {
          val img = ctx.createImageData(w, h)
          val data = img.data
          for {
            r ← 0 until n3
            c ← 0 until n3
            Color(red, green, blue) = color(c, r, n, n2)
            (x1, x2) = (c * w / n3, (c+1) * w / n3)
            (y1, y2) = (r * h / n3, (r+1) * h / n3)
            x ← x1 until x2
            y ← y1 until y2
            idx = 4 * (w * y + x)
          } {
            data(idx    ) = red
            data(idx + 1) = green
            data(idx + 2) = blue
            data(idx + 3) = 255
          }

          ctx.putImageData(img, 0, 0)
          Some(img)
        } {
          img ⇒
            ctx.putImageData(img, 0, 0)
            None
        }
    }

    /**
     * Set new dimensions for the canvas, and trigger state updates as necessary
     *
     * The <canvas> is not a well-behaved component or element; it is mutated by updating the `data` buffers of
     * [[ImageData]]s stored in [[Canvas.imgs]]
     */
    def setSize(canvas: Canvas, sz: Size): Callback = {
      val img = draw(canvas, sz)

      modState(
        img
          .fold[State ⇒ State] {
            _.copy(size = sz)
          } {
            img ⇒
              _.copy(
                canvas =
                  canvas.map(
                    c ⇒ c.copy(
                      imgs =
                        c.imgs ++ Map(sz → img)
                    )
                  ),
                size = sz
              )
          }
      )
    }

    def render(sz: Size, s: State): VdomElement = {
      val State(sz2, canvas, color) = s
      val Size(n, n2, n3, _) = sz2.getOrElse(sz)

      import Size._
      div(
        <.canvas(
          Style.canvas,
          ^.onMouseMove ==> {
            e: ReactMouseEvent ⇒
              canvas.fold { Callback() } {
                case Canvas(canvas, imgs, _, w, h) ⇒

                  val x = e.clientX - canvas.offsetLeft
                  val y = e.clientY - canvas.offsetTop

                  val c = x * n3 / w toInt
                  val r = y * n3 / h toInt

                  import hilbert._
                  val color = this.color(c, r, n, n2)

                  $.modState(_.copy(color = color))
              }
          }
        ),
        div(
          Style.panel,
          color.map {
            case Color(r, g, b) ⇒
              div(
                Style.color,
                div(
                  Style.thumb,
                  ^.backgroundColor := s"rgb($r,$g,$b)",
                ),
                pre(
                  Style.pre,
                  Seq(r, g, b)
                  .map(
                    "% 3d".format(_).takeRight(3)
                  )
                  .mkString("rgb(", ",", ")")
                ),
                pre(
                  Style.pre,
                  {
                    val hexs =
                      Seq(r, g, b)
                        .map { "%02x".format(_) }

                    (
                      // shorten to one-char hex-strings if all colors' hex-strings allow it (i.e. are two copies of the
                      // same character)
                      if (
                        hexs.forall {
                          s ⇒ s.apply(0) == s.apply(1)
                        }
                      )
                        hexs.map(_.drop(1))
                      else
                        hexs
                    )
                    .mkString("#", "", "")
                  }
                )
              )
          },
          canvas
            .map {
              canvas ⇒
                div(
                  Style.buttons,
                  button(
                    "8x8",
                    Style.sizeButton,
                    ^.onMouseEnter --> setSize(canvas, `1`)
                  ),
                  button(
                    "64x64",
                    Style.sizeButton,
                    ^.onMouseEnter --> setSize(canvas, `2`)
                  ),
                  button(
                    "512x512",
                    Style.sizeButton,
                    ^.onMouseEnter --> setSize(canvas, `3`)
                  ),
                )
            }
        )
      )
    }
  }

  val component =
    ScalaComponent
      .builder[Size]("Picker")
      .initialState(State())
      .renderBackend[Backend]
      .componentDidMount {
        p ⇒
          import p._
          val props = p.props

          val canvas =
            getDOMNode
              .node
              .asInstanceOf[Div]
              .getElementsByTagName("canvas")(0)
              .asInstanceOf[html.Canvas]

          val w = canvas.clientWidth
          val h = canvas.clientHeight

          canvas.setAttribute( "width", w.toString)
          canvas.setAttribute("height", h.toString)

          val ctx =
            canvas
              .getContext("2d")
              .asInstanceOf[CanvasRenderingContext2D]

          val sz = state.size.getOrElse(props)
          val c =
            Canvas(
              canvas,
              Map(),
              ctx,
              w,
              h
            )

          p.backend.setSize(c, sz)

          setState(
            state.copy(
              canvas = c,
              color = Color(0, 0, 0)
            )
          )
      }
      .build
}
