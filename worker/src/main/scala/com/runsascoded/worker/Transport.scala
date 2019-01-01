package com.runsascoded.worker

import boopickle.Default._
import magnolia._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray.{ ArrayBuffer, TypedArrayBuffer }
import scala.language.experimental.macros

//object Serde {
//  def apply[T](t: T)(implicit pickler: Transport[T]): ArrayBuffer = {
//    val data = Pickle.intoBytes(t)
//    assert(data.hasTypedArray())
//    data.typedArray.buffer.slice(0, data.limit())
//  }
//  def apply[T](msg: dom.MessageEvent)(implicit pickler: Pickler[T]): T = apply(msg.data.asInstanceOf[ArrayBuffer])(pickler)
//  def apply[T](data: ArrayBuffer)(implicit pickler: Pickler[T]): T = {
//    val buffer = TypedArrayBuffer.wrap(data)
//    println(s"received response: ${data.byteLength}")
//    Unpickle[T].fromBytes(buffer)
//  }
//}

trait Transport[T] {
  def apply(t: T): js.Object
  def from(e: js.Object): T
}

trait Derivations {
  type Typeclass[T] = Transport[T]
  def combine[T](cc: CaseClass[Typeclass, T]): Typeclass[T] =
    new Typeclass[T] {
      def apply(t: T): js.Object = {
        val buffers =
          cc
            .parameters
            .map {
              p ⇒
                p.typeclass(p.dereference(t)).asInstanceOf[ArrayBuffer]
            }

        js.Array(buffers: _*)
      }
      def from(e: js.Object): T =
        cc.rawConstruct(
          e
            .asInstanceOf[js.Array[js.Object]]
            .zip(cc.parameters)
            .map {
              case (buffer, p) ⇒
                p.typeclass.from(buffer)
            }
        )
    }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    new Typeclass[T] {
      val subtypes =
        sealedTrait
          .subtypes
          .map {
            subtype ⇒
              subtype.typeName.short → subtype
          }
          .toMap

      def apply(t: T): js.Object =
        sealedTrait.dispatch(t) {
          subtype ⇒
            val out = subtype.typeclass(subtype.cast(t))
            val label = subtype.typeName.short
            js.Tuple2(label, out)
        }
      def from(e: js.Object): T = {
        val js.Tuple2(label, out) = e.asInstanceOf[js.Tuple2[String, ArrayBuffer]]
        val subtype = subtypes(label)
        subtype.typeclass.from(out)
      }
    }
  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
trait LowPriorityTransport
  extends Derivations {

  implicit def fromPicker[T](implicit pickler: Pickler[T]): Transport[T] =
    new Transport[T] {
      override def apply(t: T): js.Object = {
        val data = Pickle.intoBytes(t)
        assert(data.hasTypedArray())
        data.typedArray.buffer.slice(0, data.limit())
      }
      override def from(e: js.Object): T = {
        val data = e.asInstanceOf[ArrayBuffer]
        val buffer = TypedArrayBuffer.wrap(data)
        Unpickle[T].fromBytes(buffer)
      }
    }
}
object Transport
  extends LowPriorityTransport {
  def apply[T](t: T)(implicit transport: Transport[T]): js.Object = transport(t)
  def apply[T](msg: dom.MessageEvent)(implicit transport: Transport[T]): T = apply(msg.data.asInstanceOf[ArrayBuffer])(transport)
  def apply[T](data: ArrayBuffer)(implicit transport: Transport[T]): T = transport.from(data)

  implicit def jsID[T <: js.Object]: Transport[T] =
    new Transport[T] {
      override def apply(t: T): js.Object = t
      override def from(e: js.Object): T = e.asInstanceOf[T]
    }
}
