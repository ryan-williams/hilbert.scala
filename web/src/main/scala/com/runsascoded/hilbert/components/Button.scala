package com.runsascoded.hilbert.components

import com.runsascoded.hilbert.css.Style
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.raw.HTMLElement
import scalacss.ScalaCssReact._

object Button {
  case class Props[T](
    t: T,
    pressed: Boolean,
    clicked: Boolean,
    label: String,
    fn: Mod[Preview[T]]
  )

  def apply[T] =
    ScalaComponent
      .builder[Props[T]]("button")
      .render_P {
        case Props(t, pressed, clicked, label, fn) ⇒
          button(
            label,
            Style.sizeButton,
            if (pressed) Style.pressed else Style.none,
            ^.className := s"button-$t",
            ^.onMouseEnter --> fn(_.copy(hover =    t)),
            //^.onMouseLeave --> fn(_.copy(hover = None)),
            ^.onClick      -->
              fn(
                if (clicked)
                  _.copy(click = None)
                else
                  _.copy(click =    t)
              ),
            ^.onFocus      ==> { e: ReactEvent ⇒ e.target.asInstanceOf[HTMLElement].blur(); Callback() },
          )
      }
      .build
}
