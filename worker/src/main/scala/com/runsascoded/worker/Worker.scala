package com.runsascoded.worker

import org.scalajs.dom
import org.scalajs.dom.webworkers

class Worker[
    To: Transport,
  From: Transport
](
  fn: From ⇒ Unit = null,
  script: String = "worker.js"
) {
  private val worker = new webworkers.Worker(script)
  Option(fn).foreach(onMessage)
  def onMessage(fn: From ⇒ Unit): Unit = {
    worker.onmessage =
      (e: dom.MessageEvent) ⇒
        fn(
          Transport[From](e)
        )
  }
  def !(to: To): Unit = worker.postMessage(Transport(to))
}
