package controllers

import autowireImpl.{AutoWireByteServer, ByteRouter}
import boopickle.Default._
import com.google.inject.Inject
import javax.inject._
import moleculeadmin.server.QueryBackend
import moleculeadmin.server.page.QueryPage
import moleculeadmin.shared.api.QueryApi
import org.webjars.play.WebJarsUtil
import scala.concurrent.ExecutionContext


@Singleton
class QueryController @Inject()(api: QueryBackend)(implicit webJarsUtil: WebJarsUtil, ec: ExecutionContext) extends ByteRouter {
  val router = AutoWireByteServer.route[QueryApi](api)
  type keepBooPickleImport_QueryController = PickleState

  // Actions
  def query(db: String) = Action(Ok(QueryPage(db)))
}