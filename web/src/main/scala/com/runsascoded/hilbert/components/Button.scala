package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.components.Picker.Sizes
import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix.Size
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.raw.HTMLElement
import scalacss.ScalaCssReact._

object Button {
  case class Props(
    size: Size,
    pressed: Boolean,
    label: String,
    fn: (Sizes ⇒ Sizes) ⇒ Callback
  )

  val component =
    ScalaComponent
      .builder[Props]("button")
      .render_P {
        case Props(size, pressed, label, fn) ⇒
          button(
            label,
            Style.sizeButton,
            ^.className := s"button-$size ${if (pressed) "active" else ""}",
            ^.onMouseEnter --> fn(_.copy(hover =   size)),
            ^.onMouseLeave --> fn(_.copy(hover = None )),
            ^.onClick      --> fn(_.copy(click =   size)),
            ^.onFocus      ==> { e: ReactEvent ⇒ e.target.asInstanceOf[HTMLElement].blur(); Callback() },
          )
      }
      .build
}
