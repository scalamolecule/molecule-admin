package moleculeadmin.client.app.logic.query

import org.scalajs.dom.raw._
import org.scalajs.dom.{document, window}
import scala.scalajs.js.timers.setTimeout

case class WebSocketInit(ctx: String) {

  val socket = new WebSocket(s"ws://${window.location.host}/$ctx/ws")

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
}

