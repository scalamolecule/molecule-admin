package moleculeadmin.client.app.domain.query.submenu
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.rxstuff.RxBindings
import rx.{Ctx, Rx}


case class Favorites(db: String)(implicit val ctx: Ctx.Owner)
  extends Callbacks(db) with RxBindings with SubMenuElements {

  def dynRender = Rx {
    _subMenuFavorites(
      favorites().sortBy(_.molecule),
      curMolecule.now,
      useFavCallback,
      retractFavCallback
    )
  }

}
