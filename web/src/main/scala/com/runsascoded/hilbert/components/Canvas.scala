package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix
import com.runsascoded.hilbert.mix.Size
import com.runsascoded.math.Permutation
import com.runsascoded.utils.Color
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.raw.ImageData
import org.scalajs.dom.{ CanvasRenderingContext2D, html }
import scalacss.ScalaCssReact._
import shapeless.the

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
    Mod[Page.State]
  )

  case class Canvas(
    elem: html.Canvas,
    ctx: CanvasRenderingContext2D,
    w: Int,
    h: Int,
    img: ImageData
  )

  type Imgs = Map[Drawn, ImageData]
  case class State(
    canvas: Option[Canvas] = None,
    drawn: Option[Drawn] = None,
    imgs: Imgs = Map()
  )

  class Backend($: BackendScope[Props, State]) {

    def img(
      ctx: CanvasRenderingContext2D,
      w: Int,
      h: Int
    )(
      implicit
      size: Size,
      permutation: Permutation
    ) = {
      val img = ctx.createImageData(w, h)
      val data = img.data
      val Size(n, n2, n3, _) = size
      for {
        r ← 0 until n3
        c ← 0 until n3
        Color(red, green, blue) = mix.color(c, r, n, n2)
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

    def updateDrawn(
      canvas: Canvas,
      imgs: Imgs
    )(
      implicit
      size: Size,
      permutation: Permutation
    ):
      Callback =
    {
      val Canvas(_, ctx, w, h, _) = canvas
      val drawn = the[Drawn]
      val (img, cb) =
        imgs
          .get(drawn)
          .fold[(ImageData, Callback)] {
            val img = this.img(ctx, w, h)

            println(s"new canvas: $drawn")
            (
              img,
              $.modState(
                _.copy(
                  imgs = imgs + (drawn → img)
                )
              )
            )
          } {
            img ⇒
              println(s"old canvas: $drawn")
              (
                img,
                Callback()
              )
          }

      $.modState(
        _.copy(
          canvas = canvas.copy(img = img),
          drawn = drawn
        )
      ) *> cb
    }

    def render(props: Props, state: State): VdomElement = {
      implicit val (
        size @ Size(n, n2, n3, _),
        permutation,
        modState
      )
      = props

      val drawn = Drawn(size, permutation)

      val State(canvas, _, _) = state
      for {
        Canvas(_, ctx, _, _, img) ← canvas
      } {
        println("drawing")
        ctx.putImageData(img, 0, 0)
      }

      <.canvas(
        Style.canvas,
        ^.onMouseMove ==> {
          e: ReactMouseEvent ⇒
            canvas.fold { Callback() } {
              case Canvas(canvas, _, w, h, _) ⇒

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
      .shouldComponentUpdate {
        p ⇒
          val (ps, pp, _) = p.currentProps
          val (ns, np, _) = p.nextProps
          val prev = (ps, pp)
          val next = (ns, np)
          val props = prev != next
          val state = p.currentState != p.nextState

          //println(s"Should update? ${props || state}! $prev -> $next ($props), ${p.currentState} -> ${p.nextState} ($state)")
          CallbackTo(props || state)
      }
      .componentWillReceiveProps {
        p ⇒
          implicit val (size, permutation, _) = p.nextProps
          val State(canvas, drawn, imgs) = p.state
          canvas
            .fold { Callback() } {
              canvas ⇒
                if (drawn.contains(Drawn(size, permutation)))
                  {
                    //println(s"already drawn: $drawn")
                    Callback()
                  }
                else
                  p
                    .backend
                    .updateDrawn(canvas, imgs)
            }
      }
      .componentDidMount {
        p ⇒
          import p._
          implicit val (sz, permutation, cb) = p.props

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

          val img = backend.img(ctx, w, h)

          val c =
            Canvas(
              canvas,
              ctx,
              w,
              h,
              img
            )

          backend.updateDrawn(c, state.imgs) *> modState(_.copy(canvas = c))
      }
      .build
}
