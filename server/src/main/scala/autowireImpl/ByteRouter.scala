package autowireImpl

import java.nio.ByteBuffer
import autowireImpl.AutoWireByteServer.Router
import boopickle.Default._
import play.api.mvc._
import moleculeadmin.server.utils.Tags
import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.ExecutionContext.Implicits.global


//trait ByteRouter extends InjectedController with Tags with ASTpicklers {
trait ByteRouter extends InjectedController with Tags {

  /** Instantiate router in inheriting Controller with
    * {{{
    *   val router = AutoWireServer.route[YourApi](api)
    * }}}
   *
   *  OBS: YourApi will only be available to the Router (compile) if it has
   *  - no `val` declarations
   *  - no parameterless `def` definitions
   *    (`def myDef = 7` should be `def myDef() = 7` instead)
    */
  val router: Router

  def autowireRaw(path: String): Action[RawBuffer] = Action.async(parse.raw) { implicit request =>

    // Get the request body as ByteString
    val b = request.body.asBytes(parse.UNLIMITED).get

//    println("path        : " + path.split("/").toSeq)
//    println("request     : " + request)
//    println("request body: " + request.body)
//    println("request byte: " + b.asByteBuffer)

    // Apply Request to AutoWire Router
    router(
      autowire.Core.Request(
        path.split("/").toSeq,
        Unpickle[Map[String, ByteBuffer]].fromBytes(b.asByteBuffer)
      )
    ).map { buffer =>
      val data = Array.ofDim[Byte](buffer.remaining())
      buffer.get(data)
      Ok(data)
    }
  }
}

