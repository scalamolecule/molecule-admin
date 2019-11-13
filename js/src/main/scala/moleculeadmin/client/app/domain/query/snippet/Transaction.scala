package moleculeadmin.client.app.domain.query.snippet

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


case class Transaction(db: String)(implicit ctx: Ctx.Owner) extends Base(db) {
  override type keepBooPickleImport2 = PickleState

  def snippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    curTx() match {
      case 0                         => // no entity id marked yet
      case tx if showTransaction.now =>
        val snippet = document.getElementById("txSnippetTable")
        if (snippet == null) {
          // Start fresh
          curTx() = 0
        } else {
          addTxRows("txSnippetTable", tx, 0)
        }
      case _                         => // don't update non-present txSnippet
    }
    _txSnippet("Point on tx id...")
  }

  def addTxRows(parentElementId: String, tx: Long, level: Int): Unit = {
    val snippetElement = document.getElementById(parentElementId)
    if (snippetElement != null) {
      queryWire().getTxData(db, tx, enumAttrs).call().foreach { data =>
        snippetElement.innerHTML = ""
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
          val cellType   = snippetCellTypes(attr)
          val vElementId = parentElementId + attr + v.take(20)

          val entityCell = if (i == 1)
            td(s"${curT.now} / $e", cls := "txChosen")
          else
            th(e, cls := Rx(if (e == curEntity()) "eidChosen" else "eid"),
              onmouseover := { () => curEntity() = e })

          val valueCell = getValueCell(cellType, vElementId, v, true, level, asserted)
          val attr1     = if (attrPrev != attr || ePrev != e) attr else ""
          val attrCell  = getAttrCell(attr1, cellType, vElementId, valueCell, true)
          snippetElement.appendChild(
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
