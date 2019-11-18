package controllers

import autowireImpl.{AutoWireByteServer, ByteRouter}
import boopickle.Default._
import com.google.inject.Inject
import javax.inject._
import moleculeadmin.server.Schema
import moleculeadmin.server.page.SchemaPage
import moleculeadmin.shared.api.SchemaApi
import org.webjars.play.WebJarsUtil
import scala.concurrent.ExecutionContext


@Singleton
class SchemaController @Inject()(api: Schema)(implicit webJarsUtil: WebJarsUtil, ec: ExecutionContext) extends ByteRouter {
  val router = AutoWireByteServer.route[SchemaApi](api)
  type keepBooPickleImport = PickleState

  // Actions
  def schema(db: String) = Action(Ok(SchemaPage(db)))
}
