package util.client.autowire

import java.nio.ByteBuffer
import autowire.ClientProxy
import boopickle.Default._
import org.scalajs.dom
import org.scalajs.dom.window
import util.shared.autowire.AutowireSerializers
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.typedarray._


// Generic autowire Ajax wiring base classes

case class AutowireAjax[Api](context: String) {
  private val client = AutowireClientAjax(context).apply[Api]
  def apply(): ClientProxy[Api, ByteBuffer, Pickler, Pickler] = client
}


case class AutowireClientAjax(context: String)
  extends autowire.Client[ByteBuffer, Pickler, Pickler]
    with AutowireSerializers {

  override def doCall(req: Request): Future[ByteBuffer] = {
    // Request
    dom.ext.Ajax.post(
      url = s"http://${window.location.host}/$context/" + req.path.mkString("/"),
      data = Pickle.intoBytes(req.args),
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map { r =>
      // Response
      TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer])
    }
  }
}