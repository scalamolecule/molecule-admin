package moleculeadmin.client.app.domain.query.submenu
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.SubMenuElements
import org.scalajs.dom.html.LI
import rx.{Ctx, Rx}
import scalatags.JsDom


case class Queries(db: String)(implicit val ctx: Ctx.Owner)
  extends Callbacks(db) with SubMenuElements {

  def dynRender: Rx.Dynamic[JsDom.TypedTag[LI]] = Rx {
    _subMenuQueries(
      savedQueries().sortBy(_.molecule),
      curMolecule.now,
      useQueryCallback,
      retractQueryCallback
    )
  }
}
