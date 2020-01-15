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
      var isTxData = true
      queryWire().getTxData(db, tx, enumAttrs).call().foreach { data =>
        viewElement.innerHTML = ""
        var i      = 0
        var ePrev  = 0L
        var aPrev  = ""
        var eCount = 0

        data.foreach { case (e, a, v, op) =>
          i += 1
          if (e != ePrev) {
            eCount += 1
            if (i > 1)
              isTxData = false
          }
          val entityCell =
            if (i == 1 && isTxData)
              td(s"${curT.now} / $e", cls := "txChosen")
            else if (isTxData)
              td()
            else
              th(e, cls := Rx(if (e == curEntity()) "eidChosen" else "eid"),
                onmouseover := { () => curEntity() = e })

          val cellType   = viewCellTypes(a)
          val vElementId = s"$parentElementId $a $i"
          val attr1      = if (e != ePrev || a != aPrev) a else ""
          val valueCell  = getValueCell(cellType, vElementId, v, true, level, op)
          val attrCell   = getAttrCell(attr1, cellType, vElementId, valueCell, true)
          viewElement.appendChild(
            tr(
              if (i == 1)
                cls := "first"
              else if (i > 1 && eCount % 2 == 1)
                cls := "even"
              else
                (),
              if (e != ePrev) entityCell else td(),
              attrCell,
              valueCell
              //              valueCell(if (asserted) () else cls := "retracted")
            ).render
          )

          ePrev = e
          aPrev = a
        }
      }
    }
  }
}
