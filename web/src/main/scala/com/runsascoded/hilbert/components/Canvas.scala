package com.runsascoded.hilbert.components

import boopickle.Default._
import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix
import com.runsascoded.hilbert.mix.Size
import com.runsascoded.math.Permutation
import com.runsascoded.utils.Color
import com.runsascoded.worker.Worker
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{ CanvasRenderingContext2D, ImageData, html }
import scalacss.ScalaCssReact._
import shapeless.the

import scala.collection.mutable

object Canvas {

  type Sizes = Preview[Size]
   val Sizes = Preview

  type Permutations = Preview[Permutation]
   val Permutations = Preview

  type Props = (
    Size,
    Permutation,
    Color ⇒ Callback
  )

  type To = (ImageData, Int, Int, Drawn)
  type From = (Drawn, ImageData)

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

    def render(props: Props, state: State): VdomElement = {
      implicit val (
        size @ Size(n, n2, n3, _),
        permutation,
        handleColor
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

                handleColor(color)
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
                  // correct canvas is already drawn
                  Callback()
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

          import Size._
          val msgs =
            mutable.Queue(
              (
                for {
                  size ← Seq(`1`,`2`,`3`)
                  perm ← Seq(0,1,2).permutations.map { case Seq(a, b, c) ⇒ Permutation(a, b, c) }.toVector
                } yield
                  Drawn(size, perm)
              )
              : _*
            )

          val numWorkers = 6

          val workers =
            for {
              i ← 0 until numWorkers
              worker = new Worker[To, From]
            } yield {
              worker.onMessage {
                case (drawn, img) ⇒
                  println(s"received pre-computed from worker $i: $drawn $img")
                  if (msgs.nonEmpty) {
                    val drawn = msgs.dequeue()
                    println(s"re-enqueueing with worker $i: $drawn")
                    worker ! ((ctx.createImageData(w, h), w, h, drawn))
                  }
                  modState {
                    s ⇒
                      s.copy(
                        imgs = s.imgs + (drawn → img)
                      )
                  }
                  .runNow()
              }

              worker
            }

          for { (worker, i) ← workers.zipWithIndex } {
            val drawn = msgs.dequeue()
            println(s"posting to worker $i: $drawn")
            worker ! ((ctx.createImageData(w, h), w, h, drawn))
          }
          println("done firing events")

          backend.updateDrawn(c, state.imgs) *> modState(_.copy(canvas = c))
      }
      .build
}
