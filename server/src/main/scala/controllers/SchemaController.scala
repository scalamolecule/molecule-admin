package controllers

import util.server.autowire._
import boopickle.Default._
import com.google.inject.Inject
import javax.inject._
import moleculeadmin.server.Schema
import moleculeadmin.server.page.SchemaPage
import moleculeadmin.shared.api.SchemaApi
import play.api.mvc.{Action, AnyContent}
import scala.concurrent.ExecutionContext


@Singleton
class SchemaController @Inject()(api: Schema)
  (implicit ec: ExecutionContext) extends AutowireRouter {

  val autowireRouter = AutowireServer.route[SchemaApi](api)

  // Actions
  def schema(db: String): Action[AnyContent] = Action(Ok(SchemaPage(db)))
}
