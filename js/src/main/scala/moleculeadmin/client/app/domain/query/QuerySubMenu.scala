package moleculeadmin.client.app.domain.query
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.submenu._
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.html.Span
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._


case class QuerySubMenu(db: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with SubMenuElements {

  _maxRowsSelector.onchange = _ => {
    maxRows() = _maxRowsSelector.value.toInt
    _maxRowsSelector.blur()
  }

  def dynRender: Rx.Dynamic[JsDom.TypedTag[Span]] = Rx {
    // Update whenever cache updates
    queryCache()
    span(
      _maxRowsSelector,
      ul(cls := "nav nav-pills",
        Queries(db).dynRender,
        if (modelElements().nonEmpty) {
          Seq(
            Cache(db).dynRender,
            Views().dynRender,
            ShortCuts().dynRender
          )
        } else ()
      )
    )
  }
}
