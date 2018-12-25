package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.components.Picker.{ Canvas, Size }
import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.{ three, two }
import hilbert._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.ImageData
import org.scalajs.dom.{ CanvasRenderingContext2D, html }
import scalacss.ScalaCssReact._

import scala.scalajs.js

object Picker {

//  val size = 800

  case class Color(r: Int, g: Int, b: Int)
//  sealed trait Color
//  case class RGB(r: Int, g: Int, b: Int) extends Color
//  case class HSV(h: Int, s: Int, v: Int) extends Color

  case class Canvas(
    elem: html.Canvas,
    imgs: Map[Size, ImageData],
    ctx: CanvasRenderingContext2D,
    w: Int,
    h: Int
  )

  case class State(
      size: Option[  Size] = None,
    canvas: Option[Canvas] = None,
     color: Option[ Color] = None
  )

  /**
   *
   *
   * @param n 1, 2, 3, or 4 (corresponding to 64**n colors displayed; 64**4 == 2**24 is the full 3-byte color-space
   */
  sealed abstract class Size(
    val n: Int
  ) {
    val n2 = 1 << (2*n)  // virtual RGB-cube edge-length
    val n3 = 1 << (3*n)  // color-squares per side
    val n6 = 1 << (6*n)  // total number of color-squares
  }
  object Size {
    object `1` extends Size(1)
    object `2` extends Size(2)
    object `3` extends Size(3)
    object `4` extends Size(4)
    def unapply(size: Size): Option[(Int, Int, Int, Int)] = {
      import size._
      (
        n,
        n2,
        n3,
        n6
      )
    }
  }
  implicit def liftOpt[T](t: T): Option[T] = Some(t)

  class Backend($: BackendScope[Size, State]) {
    import $.modState

    def draw(canvas: Canvas, sz: Size): Option[ImageData] = {
      val Size(n, n2, n3, _) = sz
      val Canvas(_, imgs, ctx, w, h) = canvas
      imgs
        .get(sz)
        .fold {
          val img = ctx.createImageData(w, h)
          val data = img.data
          for {
            r ← 0 until n3
            c ← 0 until n3
            three.P(_r, _g, _b) = `3`(`2`(two.P(c, r)))
            red = _r * 255 / (n2 - 1)
            green = _g * 255 / (n2 - 1)
            blue = _b * 255 / (n2 - 1)
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
          Some(img): Option[ImageData]
        } {
          img ⇒
            ctx.putImageData(img, 0, 0)
            None
        }
    }

    def setSize(canvas: Option[Canvas], sz: Size): Callback = {
      val img = canvas.flatMap {
        draw(_, sz)
      }
      modState(
        img.fold[State ⇒ State] {
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
                  val three.P(_r, _g, _b) = `3`(`2`(two.P(c, r)))
                  val   red = _r * 255 / (n2 - 1)
                  val green = _g * 255 / (n2 - 1)
                  val  blue = _b * 255 / (n2 - 1)
                  val color = Color(red, green, blue)

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
                  Seq(r, g, b)
                    .map {
                      n ⇒
                        val s = "%02x".format(n)
                        if (s.apply(0) == s.apply(1))
                          s.drop(1)
                        else
                          s
                    }
                    .mkString("#", "", "")
                )
              )
          },
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
          )
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
