package moleculeadmin.client.app.domain.query.views

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.document
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{td, tr, _}
import scala.concurrent.ExecutionContext.Implicits.global


case class Transaction()(implicit ctx: Ctx.Owner) extends Base {
  type keepBooPickleImport_Transaction = PickleState

  def view: Rx.Dynamic[TypedTag[Element]] = Rx {
    curTx() match {
      case 0 => // no entity id marked yet

      case tx if curViews.now.contains("view05_Transaction") =>
        val view = document.getElementById("txViewTable")
        if (view == null) {
          // Start fresh
          curTx() = 0
        } else {
          addTxRows("txViewTable", tx, 0)
        }

      case _ => // don't update non-present txView
    }
    _txView("Point on tx id...")
  }


  def addTxRows(parentElementId: String, tx: Long, level: Int): Unit = {
    val viewElement = document.getElementById(parentElementId)
    if (viewElement != null) {
      queryWire().getTxData(db, tx, enumAttrs).call().foreach {
        case (txInst, txMetaData, txData) =>
          viewElement.innerHTML = ""

          // :db/txInstant
          viewElement.appendChild(
            tr(
              cls := "first",
              td(s"${curT.now} / $tx", cls := "txChosen"),
              td(":db/txInstant"),
              td(txInst),
            ).render
          )

          // tx meta data
          var i      = 1
          txMetaData.foreach { case (_, a, v, op) =>
            val cellType   = viewCellTypes(a)
            val vElementId = s"$parentElementId $a $i"
            val valueCell  = getValueCell(cellType, vElementId, v, true, level, op)
            val attrCell   = getAttrCell(a, cellType, vElementId, valueCell, true)
            viewElement.appendChild(
              tr(
                td(),
                attrCell,
                valueCell
              ).render
            )
            i += 1
          }

          // tx data
          var ePrev  = tx
          var aPrev  = ""
          var eCount = 1
          txData.foreach { case (e, a, v, op) =>
            if (e != ePrev) {
              eCount += 1
            }
            val cellType   = viewCellTypes(a)
            val vElementId = s"$parentElementId $a $i"
            val attr1      = if (e != ePrev || a != aPrev) a else ""
            val valueCell  = getValueCell(cellType, vElementId, v, true, level, op)
            val attrCell   = getAttrCell(attr1, cellType, vElementId, valueCell, true)
            viewElement.appendChild(
              tr(
                if (eCount % 2 == 0) cls := "even" else (),
                if (e != ePrev)
                  th(e, cls := Rx(if (e == curEntity()) "eidChosen" else "eid"),
                    onmouseover := { () => curEntity() = e })
                else
                  td(),
                attrCell,
                valueCell
              ).render
            )
            ePrev = e
            aPrev = a
            i += 1
          }
      }
    }
  }
}
















