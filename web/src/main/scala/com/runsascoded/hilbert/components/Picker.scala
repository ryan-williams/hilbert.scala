package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix.Size
import com.runsascoded.hilbert.{ three, two }
import com.runsascoded.math.Permutation
import com.runsascoded.utils.Ints
import hilbert._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{ HTMLElement, ImageData }
import org.scalajs.dom.{ CanvasRenderingContext2D, html }
import scalacss.ScalaCssReact._
import shapeless.the

import scala.collection.mutable

object Picker {

//  val size = 800

//  sealed trait Color
//  case class RGB(r: Int, g: Int, b: Int) extends Color
//  case class HSV(h: Int, s: Int, v: Int) extends Color

  case class Preview[T](
    hover: Option[T] = None,
    click: Option[T] = None
  ) {
    def t: Option[T] = click.orElse(hover) //hover.orElse(click)
  }
  object Preview {
    implicit def unwrap[T](preview: Preview[T]): Option[T] = preview.t
  }

  type Sizes = Preview[Size]
  val Sizes = Preview

  type Permutations = Preview[Permutation]
  val Permutations = Preview

  case class State(
           sizes:          Sizes  = Sizes(),
    permutations:   Permutations  = Permutations(),
          canvas:  Option[Canvas] = None,
           color:  Option[ Color] = None
  )

  case class Drawn(size: Size, permutation: Permutation)
  object Drawn {
    implicit def wrap(implicit size: Size, permutation: Permutation): Drawn = Drawn(size, permutation)
  }

  class Backend($: BackendScope[Size, State])
    extends Ints.syntax {

    import $.modState

    var drawnStatus: Option[Drawn] = None
    val imgs = mutable.Map[Drawn, ImageData]()

    def color(c: Int, r: Int, n: Int, n2: Int)(implicit permutation: Permutation): Color = {
      val three.P(_r, _g, _b) =
        `3`(
          `2`(
            // top level should go right, down, left; this is true without a shift for n == 1 (in an 8x8 square, the top
            // level is 2x2, which is two power-of-two flips from the bottom level), and each subsequent `n` adds 3
            // powers of 2, requiring toggling the orientation
            two.P(c, r) >> (n - 1)
          )
          // Keep the highest level constant: red is the least significant axis, then green, then blue.
          // When n == 1 (4x4x4 cube), the top-level dimension is one from the RGB bottom level, namely GBR, so we shift
          // by 1.
          // Subsequent cases multiply the cube's edge-length by 4, requiring two additional shifts.
        ) >> (2*n - 1)
      Color(
        _r * 255 / (n2 - 1),
        _g * 255 / (n2 - 1),
        _b * 255 / (n2 - 1)
      )(
        permutation
      )
    }

    def draw(canvas: Canvas)(implicit size: Size, permutation: Permutation) = {
      val Size(n, n2, n3, _) = size
      val Canvas(_, ctx, w, h) = canvas
      val drawn = the[Drawn]
      val img =
        imgs
          .getOrElseUpdate(
            drawn,
            {
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

              img
            }
          )

      ctx.putImageData(img, 0, 0)
      drawnStatus = Some(drawn)
    }

    def render(props: Size, s: State): VdomElement = {
      val State(sizes, permutations, canvas, color) = s
      implicit val size @ Size(n, n2, n3, _) = sizes.t.getOrElse(props)
      implicit val permutation = permutations.t.getOrElse(Permutation(0, 1, 2))

      for {
        canvas ← canvas
        if !drawnStatus.contains(Drawn(size, permutation))
      } {
        draw(canvas)
      }

      import Size._
      div(
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

          val div =
            getDOMNode
              .node
              .asInstanceOf[Div]

          val canvas =
            div
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

          val sz = state.sizes.t.getOrElse(props)

          val c =
            Canvas(
              canvas,
              ctx,
              w,
              h
            )

          setState(
            state.copy(
              canvas = c,
              color = Color(0, 0, 0),
              sizes = Sizes(hover = sz),
              permutations = Permutations(hover = Permutation(0, 1, 2))
            )
          )
      }
      .build
}
