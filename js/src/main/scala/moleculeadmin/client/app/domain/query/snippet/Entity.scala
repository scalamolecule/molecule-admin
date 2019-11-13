package moleculeadmin.client.app.domain.query.snippet
import moleculeadmin.client.app.domain.query.QueryState._
import org.scalajs.dom.document
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag


case class Entity(db: String)(implicit ctx: Ctx.Owner) extends Base(db) {

  def snippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    curEntity() match {
      case 0                     => // no entity id marked yet
      case eid if showEntity.now =>
        val snippet = document.getElementById("entitySnippetTable")
        if (snippet == null) {
          // Start fresh
          curEntity() = 0
        } else {
          addEntityRows("entitySnippetTable", eid, false, 0)
        }
      case _                     => // don't update non-present entitySnippet
    }
    _entitySnippet("Point on entity id...")
  }
}
