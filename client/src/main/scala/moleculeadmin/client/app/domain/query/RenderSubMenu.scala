package moleculeadmin.client.app.domain.query

import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.submenu.{SubMenuGrouped, SubMenuNew, SubMenuQueryList, SubMenuShortCuts, SubMenuUndo, SubMenuViews}
import moleculeadmin.client.app.element.query.SubMenuElements
import org.scalajs.dom.html.Span
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._


case class RenderSubMenu()(implicit val ctx: Ctx.Owner)
  extends SubMenuElements {

  // Initialize max row onchange callback
  _maxRowsSelector.onchange = _ => {
    maxRows() = _maxRowsSelector.value.toInt
    (new Callbacks).saveSetting("maxRows" -> maxRows.now.toString)
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
        SubMenuShortCuts().render
      )
    )
  }
}
