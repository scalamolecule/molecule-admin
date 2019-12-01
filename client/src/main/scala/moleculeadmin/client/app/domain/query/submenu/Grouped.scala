package moleculeadmin.client.app.domain.query.submenu

import autowire._
import boopickle.Default._
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


case class Grouped()(implicit val ctx: Ctx.Owner)
  extends RxBindings with SubMenuElements {

  type keepBooPickleImport_Views = PickleState

  def cb(colIndex: Int, attr: Frag, isOn: Boolean): TypedTag[Div] = Rx {
    _cb(
      "grouped-" + colIndex,
      attr,
      groupedCols.now.contains(colIndex),
      { () =>
        val groupedOnBefore = showGrouped.now
        val isOn            = groupedCols.now.contains(colIndex)
        if (colIndex == -1) {
          showGrouped() = !showGrouped.now
        } else {
          if (isOn) {
            groupedCols() = groupedCols.now.filterNot(_ == colIndex)
            if (groupedCols.now.isEmpty) {
              document.getElementById("checkbox-grouped--1")
                .asInstanceOf[HTMLInputElement].checked = false
              showGrouped() = false
            }
          } else {
            if (groupedCols.now.isEmpty) {
              document.getElementById("checkbox-grouped--1")
                .asInstanceOf[HTMLInputElement].checked = true
              showGrouped() = true
            }
            groupedCols() = groupedCols.now :+ colIndex
          }
        }
        //        if (groupedOn.now != groupedOnBefore) {
        //          queryWire().saveSetting("groupedOn", groupedOn.now.toString)
        //            .call().foreach {
        //            case Left(err) => window.alert(err)
        //            case Right(_)  => println(
        //              "Saved setting for `groupedOn`: " + groupedOn.now
        //            )
        //          }
        //        }
      }
    )
  }.now


  def dynRender: Rx.Dynamic[TypedTag[LI]] = Rx {
    _subMenu(
      "Grouped",
      Seq(
        cb(-1, span("Show ", span("G", textDecoration.underline), "rouped attributes"), showGrouped.now),
        hr
      ) ++ groupableCols.map { colIndex =>
        val Col(_, _, nsAlias, _, attr, _, _, _, _, _, _, _, _, _, _) =
          columns.now.find(_.colIndex == colIndex).get

        val isOn = groupedCols.now.contains(colIndex)
        cb(colIndex, s"$nsAlias/$attr", isOn)
      }
    )
  }
}
