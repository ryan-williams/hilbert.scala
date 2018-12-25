package com.runsascoded.hilbert

import com.runsascoded.hilbert.components.Picker
import com.runsascoded.hilbert.components.Picker.Size._
import com.runsascoded.hilbert.css.Style
import org.scalajs.dom._
import scalacss.DevDefaults._
import document.getElementById

object Main {
  def main(args: Array[String]): Unit = {
    Style.addToDocument()
    Picker.component(`1`) renderIntoDOM getElementById("container")
  }
}
