package moleculeadmin.client.app.domain.query.submenu

import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.html.{Div, LI}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


case class SubMenuGrouped()(implicit val ctx: Ctx.Owner)
  extends Callbacks with RxBindings with SubMenuElements {

  def cb(n: Int, colIndex: Int, attr: String): TypedTag[Div] = Rx {
    if(n == 0)
      _cb(
        "grouped-showGrouped",
        _cbLabel("␣", attr),
        showGrouped,
        () => toggleShowGrouped()
      )
    else
      _cb(
        "grouped-" + colIndex,
        _cbLabel(s"$n", attr),
        groupedCols.now.contains(colIndex),
        () => toggleGrouped(colIndex)
      )
  }.now


  def dynRender: Rx.Dynamic[TypedTag[LI]] = Rx {
    groupableCols()
    _subMenu(
      "submenu-grouped",
      _shortcut("G", "rouped"),
      Seq(
        cb(0, -1, "Grouped Attributes"),
        hr
      ) ++ groupableCols.now.zipWithIndex.map { case (colIndex, i) =>
        val c = columns.now.find(_.colIndex == colIndex).get
        cb(i + 1, c.colIndex, s"${c.nsAlias}/${c.attr}")
      }
    )
  }
}
