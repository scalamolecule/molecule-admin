package moleculeadmin.client.app.logic.query

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.submenu._
import moleculeadmin.client.app.html.query.SubMenuElements
import org.scalajs.dom.html.Span
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._


case class RenderSubMenu()(implicit val ctx: Ctx.Owner)
  extends SubMenuElements {

  // Initialize max row onchange callback
  _maxRowsSelector.onchange = _ => {
    maxRows() = _maxRowsSelector.value.toInt
    new Callbacks().saveSetting("maxRows" -> maxRows.now.toString)
    _maxRowsSelector.blur()
  }

  def dynRender: Rx.Dynamic[JsDom.TypedTag[Span]] = Rx {
    renderSubMenu()
    span(
      _maxRowsSelector,
      ul(
        cls := "nav nav-pills",
        SubMenuQueryList().render,
        SubMenuNew().render,
        SubMenuUndo().render,
        SubMenuGrouped().render,
        SubMenuViews().render,
        SubMenuShortCuts().render,
        SubMenuEntityLock().render
      )
    )
  }
}
