package moleculeadmin.client.app.domain.query

import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.submenu.{SubMenuGrouped, SubMenuQueryList, SubMenuShortCuts, SubMenuViews}
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
    //    println("RenderSubMenu")
    span(
      _maxRowsSelector,
      ul(cls := "nav nav-pills",
        if (
          savedQueries.nonEmpty
            || recentQueries.nonEmpty
            || modelElements.now.nonEmpty
        ) SubMenuQueryList().render else (),

        if (modelElements.now.nonEmpty && rowCountAll > 0) {
          Seq(
            SubMenuGrouped().render,
            SubMenuViews().render,
          )
        } else (),

        SubMenuShortCuts().render
      )
    )
  }
}
