package moleculeadmin.client.app.domain.query.views
import moleculeadmin.client.app.domain.query.QueryState._
import org.scalajs.dom.document
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag


case class Entity(db: String)(implicit ctx: Ctx.Owner) extends Base(db) {

  def view: Rx.Dynamic[TypedTag[Element]] = Rx {
    curEntity() match {
      case 0 => // no entity id marked yet

      case eid if curViews.now.contains("viewEntity") =>
        val view = document.getElementById("entityViewTable")
        if (view == null) {
          // Start fresh
          curEntity() = 0
        } else {
          addEntityRows("entityViewTable", eid, false, 0)
        }

      case _ => // don't update non-present entityView
    }
    _entityView("Point on entity id...")
  }
}
