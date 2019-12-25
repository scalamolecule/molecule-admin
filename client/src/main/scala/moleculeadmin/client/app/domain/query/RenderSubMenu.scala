package moleculeadmin.client.app.domain.query
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.submenu.{SubMenuQueryList, _}
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.html.Span
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._


case class RenderSubMenu(db: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with SubMenuElements {

  _maxRowsSelector.onchange = _ => {
    maxRows() = _maxRowsSelector.value.toInt
    (new Callbacks).saveSetting("maxRows" -> maxRows.now.toString)
    _maxRowsSelector.blur()
  }


  def dynRender: Rx.Dynamic[JsDom.TypedTag[Span]] = Rx {
//    println("---- submenu")
    // Re-calc sub menu when query changes
    curMolecule()

    span(
      _maxRowsSelector,
      ul(cls := "nav nav-pills",
        if (
          savedQueries.nonEmpty
            || recentQueries.nonEmpty
            || modelElements.now.nonEmpty
        ) SubMenuQueryList(db).dynRender else (),

        if (modelElements.now.nonEmpty) {
          Seq(
            SubMenuGrouped().dynRender,
            SubMenuViews().dynRender,
          )
        } else (),

        SubMenuShortCuts().dynRender
      )
    )
  }
}
