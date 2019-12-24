package moleculeadmin.client.app.domain.query.submenu

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.Col
import org.scalajs.dom.html.{Div, LI}
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{document, window}
import rx.{Ctx, Rx, Var}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class SubMenuGrouped()(implicit val ctx: Ctx.Owner)
  extends Callbacks with RxBindings with SubMenuElements {

  type keepBooPickleImport_Views = PickleState

  def cb(colIndex: Int, shortCut: Int, attr: String, isOn: Boolean): TypedTag[Div] = Rx {
    _cb(
      "grouped-" + colIndex,
      _cbLabel(if (shortCut == 0) "␣" else s"$shortCut", attr),
      isOn,
      if (shortCut == 0)
        () => toggleShowGrouped()
      else
        () => toggleGrouped(colIndex)
    )
  }.now


  def dynRender: Rx.Dynamic[TypedTag[LI]] = Rx {
    groupCols()
    //    println("SubMenuGrouped: " + showGrouped + " - " + groupCols.now + " - " + groupedCols.now)
    _subMenu(
      "submenu-grouped",
      _shortcut("G", "rouped"),
      Seq(
        cb(-1, 0, "Grouped Attributes", showGrouped),
        hr
      ) ++ groupCols.now.zipWithIndex.map { case (colIndex, i) =>
        val Col(_, _, nsAlias, _, attr, _, _, _, _, _, _, _, _, _, _) =
          columns.now.find(_.colIndex == colIndex).get

        val isOn = groupedCols.now.contains(colIndex)
        cb(colIndex, i + 1, s"$nsAlias/$attr", isOn)
      }
    )
  }
}
