package controllers

import util.server.autowire._
import boopickle.Default._
//import com.google.inject.Inject
import javax.inject._
import moleculeadmin.server.QueryBackend
import moleculeadmin.server.page.QueryPage
import moleculeadmin.shared.api.QueryApi
import play.api.mvc.{Action, AnyContent}
import scala.concurrent.ExecutionContext

@Singleton
class QueryController @Inject()(api: QueryBackend)(implicit ec: ExecutionContext)
  extends AutowireController {

  // Auto-wired actions
  val autowireRouter = AutowireServer.route[QueryApi](api)

  // Explicit actions
  def query(db: String): Action[AnyContent] = Action(
    Ok(QueryPage(db))
//      .withSession("prevAjaxCount" -> "0")
  )
}