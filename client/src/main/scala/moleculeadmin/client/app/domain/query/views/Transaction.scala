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

      case tx if curViews.now.contains("view03_Transaction") =>
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
      queryWire().getTxData(db, tx, enumAttrs).call().foreach { data =>
        viewElement.innerHTML = ""
        var i        = 0
        var ePrev    = 0L
        var eCur     = 0L
        var eCount   = 0
        var attrPrev = ""
        data.foreach { case (e, attr, v, asserted) =>
          i += 1
          if (e != ePrev) {
            eCur = e
            eCount += 1
          }
          val cellType   = viewCellTypes(attr)
          val vElementId = parentElementId + attr + v.take(20)

          val entityCell = if (i == 1)
            td(s"${curT.now} / $e", cls := "txChosen")
          else
            th(e, cls := Rx(if (e == curEntity()) "eidChosen" else "eid"),
              onmouseover := { () => curEntity() = e })

          val valueCell = getValueCell(cellType, vElementId, v, true, level, asserted)
          val attr1     = if (attrPrev != attr || ePrev != e) attr else ""
          val attrCell  = getAttrCell(attr1, cellType, vElementId, valueCell, true)
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
          attrPrev = attr
        }
      }
    }
  }

}
