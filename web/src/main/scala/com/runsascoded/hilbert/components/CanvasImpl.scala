package com.runsascoded.hilbert.components

import boopickle.Default._
import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix
import com.runsascoded.hilbert.mix.Size
import com.runsascoded.math.Permutation
import com.runsascoded.worker.{ Transport, Worker }
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.{ CanvasRenderingContext2D, ImageData, html }
import scalacss.ScalaCssReact._
import shapeless.the

import scala.scalajs.js

object CanvasImpl {

  type Sizes = Preview[Size]
   val Sizes = Preview

  type Permutations = Preview[Permutation]
   val Permutations = Preview

  type Props = (
    Size,
    Permutation,
    Mod[Page.State]
  )

  type To = (ImageData, Int, Int, Drawn)
  type From = (Drawn, ImageData)

  import shapeless.the
  the[Transport[ImageData]]
  the[Transport[(ImageData, Int, Int, Drawn)]]

  val worker = new Worker[To, From]

  class Backend($: BackendScope[Props, CanvasState]) {

    val backend = this

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
            val img = Img(ctx, w, h)

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

    def render(props: Props, state: CanvasState): VdomElement = {
      implicit val (
        size @ Size(n, n2, n3, _),
        permutation,
        modState
      )
      = props

      val drawn = Drawn(size, permutation)

      val CanvasState(canvas, _, _) = state
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
      .initialState(CanvasState())
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
          val CanvasState(canvas, drawn, imgs) = p.state
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

          val img = Img(ctx, w, h)

          val c =
            Canvas(
              canvas,
              ctx,
              w,
              h,
              img
            )

          worker.onMessage {
            case (drawn, img) ⇒
            println(s"received pre-computed: $drawn $img")
            modState {
              s ⇒
                s.copy(
                  imgs = s.imgs + (drawn → img)
                )
            }
            .runNow()
          }

          import Size._
          for {
            size ← Seq(`1`,`2`,`3`).slice(1, 2)
            perm ← Seq(0,1,2).permutations.map { case Seq(a, b, c) ⇒ Permutation(a, b, c) }.toVector
            drawn = Drawn(size, perm)
          } {
            println(s"posting: $drawn")
            worker ! ((ctx.createImageData(w, h), w, h, drawn))
          }
          println("done firing events")

          backend.updateDrawn(c, state.imgs) *> modState(_.copy(canvas = c))
      }
      .build
}
