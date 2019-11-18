package moleculeadmin.client.autowire

import java.nio.ByteBuffer
import boopickle.Default._
import moleculeadmin.shared.api.{DbsApi, QueryApi, SchemaApi}
import org.scalajs.dom
import moleculeadmin.shared.api.{DbsApi, SchemaApi}
//import moleculeadmin.shared.boopickle.ASTpicklers
//import moleculeadmin.shared.boopickle.ASTpicklers
//import upickle._
//import upickle.default._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}

object dbsWire {def apply() = ByteClient("dbs")[DbsApi]}
object schemaWire {def apply() = ByteClient("schema")[SchemaApi]}
object queryWire {def apply() = ByteClient("query")[QueryApi]}

case class ByteClient(context: String) extends autowire.Client[ByteBuffer, Pickler, Pickler] {
  override def doCall(req: Request): Future[ByteBuffer] = {
    dom.ext.Ajax.post(
      url = s"http://localhost:9001/$context/" + req.path.mkString("/"),
      data = Pickle.intoBytes(req.args),
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
  }

  override def read[Result: Pickler](p: ByteBuffer) = Unpickle[Result].fromBytes(p)
  override def write[Result: Pickler](r: Result) = Pickle.intoBytes(r)
}