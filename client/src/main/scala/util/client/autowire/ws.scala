package util.client.autowire

import java.nio.ByteBuffer
import autowire.ClientProxy
import boopickle.Default._
import org.scalajs.dom.raw.{CloseEvent, Event, MessageEvent, WebSocket}
import org.scalajs.dom.window
import util.shared.autowire.AutowireSerializers
import scala.concurrent.{Future, Promise}
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._


// Generic autowire WebSocket wiring base classes

case class AutowireWebSocket[Api](ctx: String) {
  private var socket: WebSocket = _
  private var client            = newClient

  def apply(): ClientProxy[Api, ByteBuffer, Pickler, Pickler] = {
    socket.readyState match {
      case WebSocket.CLOSING | WebSocket.CLOSED =>
        client = newClient
        client
      case _                                    =>
        client
    }
  }

  def newSocket: WebSocket = {
    val socket = new WebSocket(s"ws://${window.location.host}/$ctx/ws")
    socket.binaryType = "arraybuffer"
    socket.onerror = { (e: Event) =>
      println(s"WebSocket error: $e!")
      socket.close(0, e.toString)
    }
    socket.onclose = { (e: CloseEvent) =>
      println("WebSocket closed. Reason   : " + e.reason)
      println("WebSocket closed. Was clean: " + e.wasClean)
      println("WebSocket closed. Code     : " + e.code)
    }
    socket
  }

  def newClient: ClientProxy[Api, ByteBuffer, Pickler, Pickler] = {
    socket = newSocket
    AutowireClientWebSocket(socket).apply[Api]
  }
}


case class AutowireClientWebSocket(socket: WebSocket)
  extends autowire.Client[ByteBuffer, Pickler, Pickler]
    with AutowireSerializers {

  override def doCall(req: Request): Future[ByteBuffer] = {
    // Request
    socket.readyState match {
      case WebSocket.OPEN =>
        socket.send(
          Pickle.intoBytes((req.path, req.args)).typedArray().buffer
        )

      case WebSocket.CONNECTING =>
        println("WebSocket connecting...")
        socket.onopen = { (_: Event) =>
          println("WebSocket connected")
          socket.send(
            Pickle.intoBytes((req.path, req.args)).typedArray().buffer
          )
        }

      case _ =>
        throw new IllegalStateException("Unexpected close/closing WebSocket")
    }

    // Response
    val promise = Promise[ByteBuffer]
    socket.onmessage = {
      (e: MessageEvent) =>
        promise.trySuccess(
          TypedArrayBuffer.wrap(e.data.asInstanceOf[ArrayBuffer])
        )
    }
    promise.future
  }
}