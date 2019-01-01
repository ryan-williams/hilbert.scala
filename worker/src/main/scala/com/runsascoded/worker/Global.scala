package com.runsascoded.worker

import scala.scalajs.js

@js.annotation.JSGlobalScope
@js.native
object Global extends js.Any {
  def addEventListener(`type`: String, f: js.Function): Unit = js.native
  def postMessage(data: js.Any): Unit = js.native
}
