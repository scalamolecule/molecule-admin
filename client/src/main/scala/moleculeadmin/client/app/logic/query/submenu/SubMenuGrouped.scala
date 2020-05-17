package moleculeadmin.client.app.logic.query.submenu

import moleculeadmin.client.app.logic.query.Callbacks
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.html.query.SubMenuElements
import org.scalajs.dom.html.LI
import rx.Ctx
import scalatags.JsDom.all._


case class SubMenuGrouped()(implicit val ctx: Ctx.Owner)
  extends Callbacks with SubMenuElements {


  def render: LI = {
    if (cachedQueryResult != null && cachedQueryResult.rowCountAll == 0) {
      _subMenu(
        "submenu-grouped",
        _shortcut("G", "rouped"),
        Seq(span("Can't group attributes of empty result"))
      )

    } else if (groupableCols.isEmpty) {
      _subMenu(
        "submenu-grouped",
        _shortcut("G", "rouped"),
        Seq(span("Add `e` first of a namespace to allow grouped attrs"))
      )

    } else {
      _subMenu(
        "submenu-grouped",
        _shortcut("G", "rouped"),
        Seq(
          _cb(
            "grouped-showGrouped",
            _cbLabel("␣", "Grouped Attributes"),
            showGrouped,
            () => toggleShowGrouped()
          ),
          hr
        ) ++ groupableCols.zipWithIndex.map { case (c, i) =>
          _cb(
            "grouped-" + c.colIndex,
            _cbLabel(s"${i + 1}", s"${c.nsAlias}/${c.attr}"),
            groupedColIndexes.now.contains(c.colIndex),
            () => toggleGrouped(c)
          )
        }
      )
    }
  }.render
}
