package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix
import com.runsascoded.hilbert.mix.Size
import com.runsascoded.math.Permutation
import com.runsascoded.utils.Color
import hammerlab.show._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLElement
import scalacss.ScalaCssReact._

object Image {
  implicit val showPermutations: Show[Permutation] = {
    case Permutation(a, b, c) ⇒
      Seq(a, b, c).map("rgb".apply(_)).mkString
  }

  case class Rect(x: Double, y: Double, w: Double, h: Double)
  object Rect {
    def apply(e: HTMLElement): Rect =
      Rect(
        e.offsetLeft,
        e.offsetTop,
        e.offsetWidth,
        e.offsetHeight
      )
    def unapply(e: HTMLElement): Option[(Double, Double, Double, Double)] = {
      val Rect(x, y, w, h) = Rect(e)
      Some((x, y, w, h))
    }
  }

  val component =
    ScalaComponent
      .builder[(Size, Permutation, Color ⇒ Callback)]("Image")
      .initialState[Option[Rect]](None)
      .renderPS {
        case (
          ctx,
          (Size(n, n2, n3, _), permutation, cb),
          rect
        ) ⇒
          <.img(
            Style.image,
            ^.src := show"./imgs/hilbert-$n3-512-$permutation.jpg",
            ^.onMouseMove ==> {
              e: ReactMouseEvent ⇒
                rect.fold { Callback() } {
                  case rect @ Rect(x, y, w, h) ⇒
                    val Rect(x, y, w, h) = e.target.asInstanceOf[html.Image]

                    val c = ((e.clientX - x) * n3 / w) toInt
                    val r = ((e.clientY - y) * n3 / h) toInt

                    val color = mix.color(c, r, n, n2)(permutation)
                    cb(color)
                }
            },
            ^.onLoad ==> {
              e: ReactEvent ⇒
                val img = e.target.asInstanceOf[html.Image]
                val offsets = Seq(img.offsetLeft, img.offsetTop, img.offsetWidth, img.offsetHeight).mkString("(",",",")")
                val clients = Seq(img.clientLeft, img.clientTop, img.clientWidth, img.clientHeight).mkString("(",",",")")
                println(
                  s"loaded: ${e.target} ${Rect(img)} $offsets $clients")
                ctx.setState(Some(Rect(img)))
            }
          )
      }
      .build
}
