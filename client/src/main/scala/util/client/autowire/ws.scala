package util.client.autowire

import java.nio.ByteBuffer
import autowire.ClientProxy
import boopickle.Default._
import moleculeadmin.shared.api.{DbsApi, QueryApi, SchemaApi}
import org.scalajs.dom
import org.scalajs.dom.raw.{CloseEvent, Event, MessageEvent, WebSocket}
import org.scalajs.dom.window
import util.shared.autowire.AutowireSerializers
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._


// Generic autowire WebSocket wiring base classes

case class AutowireWebSocket[Api](ctx: String) {

  def apply(): ClientProxy[Api, ByteBuffer, Pickler, Pickler] = client

  private val socket = new WebSocket(s"ws://${window.location.host}/$ctx/ws")

  socket.binaryType = "arraybuffer"

  socket.onerror = { (e: Event) =>
    println(s"exception with websocket: $e!")
    socket.close(0, e.toString)
  }
  socket.onopen = { (_: Event) =>
    println("websocket open!")
  }
  socket.onclose = { (e: CloseEvent) =>
    println("closed socket. Err: " + e.reason)
    //    setTimeout(1000) {
    //      connectWS() // try to reconnect automatically
    //    }
  }

  private val client = AutowireClientWebSocket(socket).apply[Api]
}


case class AutowireClientWebSocket(socket: WebSocket)
  extends autowire.Client[ByteBuffer, Pickler, Pickler]
    with AutowireSerializers {

  override def doCall(req: Request): Future[ByteBuffer] = {
    // Request
    socket.send(
      Pickle.intoBytes((req.path, req.args)).typedArray().buffer
    )

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