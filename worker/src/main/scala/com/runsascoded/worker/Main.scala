package com.runsascoded.worker

import org.scalajs.dom

abstract class Main[In: Transport, Out: Transport] {
  def main(args: Array[String]): Unit = {
    println("WorkerMain!")
    Global.addEventListener("message", onMessage _ )
  }

  private def onMessage(e: dom.MessageEvent): Unit = handle(Transport[In](e))

  def handle(t: In): Unit

  def post(out: Out): Unit = Global.postMessage(Transport(out))
}
