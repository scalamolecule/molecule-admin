package moleculeadmin.client.app.domain.query.submenu
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.rxstuff.RxBindings
import rx.{Ctx, Rx}


case class Cache(db: String)(implicit val ctx: Ctx.Owner)
  extends Callbacks(db) with RxBindings with SubMenuElements {

  def dynRender = Rx {
    _subMenuCache(
      queryCache.now.map(_.molecule).sorted,
      curMolecule.now,
      useCachedCallback,
      favorites.now.map(_.molecule),
      addFavCallback,
      removeCachedCallback
    )
  }
}
