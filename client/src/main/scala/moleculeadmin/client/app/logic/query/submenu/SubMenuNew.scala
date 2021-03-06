package moleculeadmin.client.app.logic.query.submenu

import moleculeadmin.client.app.logic.query.{Callbacks, KeyEvents}
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.html.query.SubMenuElements
import org.scalajs.dom.html.LI
import rx.Ctx
import scalatags.JsDom.all._
import org.scalajs.dom.document


case class SubMenuNew()(implicit val ctx: Ctx.Owner)
  extends Callbacks with SubMenuElements with KeyEvents {


  def render: LI = {
    if (groupableCols.isEmpty) {
      _subMenu(
        "submenu-grouped",
        _shortcut("N", "ew"),
        Seq(span("Add `e` first to allow inserting new data"))
      )

    } else {
      li(
        paddingLeft := 12,
        _shortcut("N", "ew"),
        onclick := { () =>
          addInsertNewDataRow()
        }
      )
    }
  }.render
}
