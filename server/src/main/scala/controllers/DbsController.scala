package controllers

import util.server.autowire._
import boopickle.Default._
import com.google.inject.Inject
import javax.inject._
import moleculeadmin.server.Dbs
import moleculeadmin.server.page.DbsPage
import moleculeadmin.shared.api.DbsApi
import play.api.mvc.{Action, AnyContent}
import scala.concurrent.ExecutionContext


@Singleton
class DbsController @Inject()(api: Dbs)
  (implicit ec: ExecutionContext) extends AutowireController {

  val autowireRouter = AutowireServer.route[DbsApi](api)

  // Actions
  def index: Action[AnyContent] = Action(Ok(DbsPage()))
}
