package moleculeadmin.client.app.domain.query.submenu
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.rxstuff.RxBindings
import rx.{Ctx, Rx}


case class Queries(db: String)(implicit val ctx: Ctx.Owner)
  extends Callbacks(db) with SubMenuElements {

  def dynRender = Rx {
    _subMenuQueries(
      savedQueries().sortBy(_.molecule),
      curMolecule.now,
      useQueryCallback,
      retractQueryCallback
    )
  }

}
