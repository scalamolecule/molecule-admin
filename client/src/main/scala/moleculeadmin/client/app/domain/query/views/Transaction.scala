package moleculeadmin.client.app.domain.query.views

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.document
import org.scalajs.dom.html.Element
import org.scalajs.dom.raw.{Element => RawElement}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{td, tr, _}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.timers.setTimeout


case class Transaction()(implicit ctx: Ctx.Owner) extends Base {
  type keepBooPickleImport_Transaction = PickleState

  val txViewTable = "txViewTable"

  def view: Rx.Dynamic[TypedTag[Element]] = Rx {
    curTx() match {
      case 0  => // render "Point on tx id..."
      case tx => setTimeout(1) {
        // Placeholder view should be available by now
        val view = document.getElementById(txViewTable)
        if (view == null) {
          // Re-try if render isn't finished yet
          curTx.recalc()
        } else {
          val eidSpan = document.getElementById("txViewEid")
          eidSpan.innerHTML = ""
          eidSpan.appendChild(
            s"${curT.now} / ${curTx.now}".render
          )
          addTxRows(view, tx, 0)
        }
      }
    }
    _txView("Point on tx id...")
  }


  def addTxRows(view: RawElement, tx: Long, level: Int): Unit = {
    _spinTxView(view, "Fetching tx data...")

    if (view != null) {
      queryWire().getTxData(db, tx, enumAttrs).call().foreach {
        case (txInst, txMetaData, txData) =>
          view.innerHTML = ""

          // :db/txInstant
          view.appendChild(
            tr(
              cls := "first",
              td(s"${curT.now} / $tx", cls := "txChosen"),
              td(":db/txInstant"),
              td(txInst),
            ).render
          )

          // tx meta data
          var i = 1
          txMetaData.foreach { case (_, a, v, op) =>
            val cellType   = viewCellTypes(a)
            val vElementId = s"$txViewTable $a $i"
            val valueCell  = getValueCell(cellType, vElementId, v, true, level, op)
            val attrCell   = getAttrCell(a, cellType, vElementId, valueCell, true)
            view.appendChild(
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
            val vElementId = s"$txViewTable $a $i"
            val attr1      = if (e != ePrev || a != aPrev) a else ""
            val valueCell  = getValueCell(cellType, vElementId, v, true, level, op)
            val attrCell   = getAttrCell(attr1, cellType, vElementId, valueCell, true)
            view.appendChild(
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
















