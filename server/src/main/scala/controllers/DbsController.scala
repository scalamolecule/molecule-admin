package controllers

import autowireImpl.{AutoWireByteServer, ByteRouter}
import boopickle.Default._
import com.google.inject.Inject
import javax.inject._
import moleculeadmin.server.Dbs
import moleculeadmin.server.page.DbsPage
import moleculeadmin.shared.api.DbsApi
import org.webjars.play.WebJarsUtil
import scala.concurrent.ExecutionContext


@Singleton
class DbsController @Inject()(api: Dbs)(implicit webJarsUtil: WebJarsUtil, ec: ExecutionContext) extends ByteRouter {
  val router = AutoWireByteServer.route[DbsApi](api)
  type keepBooPickleImport_DbsController = PickleState

  // Actions
//  def index = Action(Ok(DbsPage(api.content)))
  def index = Action(Ok(DbsPage()))
}
