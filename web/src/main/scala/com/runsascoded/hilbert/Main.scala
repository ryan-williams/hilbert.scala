package com.runsascoded.hilbert

import com.runsascoded.hilbert.components.Page
import com.runsascoded.hilbert.css.Style
import com.runsascoded.hilbert.mix.Size.`1`
import org.scalajs.dom.document.getElementById
import scalacss.DevDefaults._

import scala.scalajs.js.annotation.{ JSExport, JSExportTopLevel }

@JSExportTopLevel("Hilbert")
object Main {
  @JSExport("main")
  def main(args: Array[String]): Unit = {
    Style.addToDocument()
    Page.component(`1`) renderIntoDOM getElementById("container")
  }
}
