package moleculeadmin.client.app.logic.query.views

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.queryWireAjax
import org.scalajs.dom.document
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.timers.setTimeout

case class EntityHistory()(implicit ctx: Ctx.Owner) extends Base {

  def view: Rx.Dynamic[TypedTag[Element]] = Rx {
    (curEntity(), entityHistorySort()) match {
      case (0, _) => // no entity id marked yet

      case (eid, sort) => setTimeout(1) {
        // Placeholder view should be available by now
        val eidSpan = document.getElementById("entityHistoryEid")
        if (eidSpan == null) {
          // Re-try if render isn't finished yet
          curEntity.recalc()
        } else {
          eidSpan.innerHTML = ""

          val byTx = if (sort == "tx")
            span("tx")
          else
            a(href := "#", "tx", onclick := { () =>
              saveSetting("entityHistorySort" -> "tx")
              entityHistorySort() = "tx"
            })

          val byAttr = if (sort == "attr")
            span("attr", paddingRight := 20)
          else
            a(href := "#", "attr", paddingRight := 20, onclick := { () =>
              saveSetting("entityHistorySort" -> "attr")
              entityHistorySort() = "attr"
            })

          eidSpan.appendChild(
            span(byTx, " | ", byAttr, eid.toString).render
          )
          addEntityHistoryRows("entityHistoryViewTable", eid, sort)
        }
      }
    }
    _entityHistoryView("Point on entity id...")
  }


  def addEntityHistoryRows(parentElementId: String, eid: Long, sort: String): Unit = {
    val viewElement = document.getElementById(parentElementId)
    if (viewElement != null) {
      queryWireAjax().getEntityHistory(db, eid, enumAttrs).call().foreach { data =>
        viewElement.innerHTML = ""
        var i      = 0
        var txPrev = 0L

        if (sort == "tx") {
          var txCur   = 0L
          var txCount = 0
          data.sortBy(t => (t._1, t._5, t._4, t._6)).foreach {
            case (t, tx, txInstant, op, a, v) if viewCellTypes.contains(a) => {
              i += 1
              if (tx != txPrev) {
                txCur = tx
                txCount += 1
              }
              val txCell     = td(
                s"$t / $tx",
                cls := Rx(if (tx == curTx()) "txChosen" else "tx"),
                onmouseover := { () =>
                  curT() = t
                  curTx() = tx
                  curTxInstant() = txInstant
                }
              )
              val cellType   = viewCellTypes(a)
              val vElementId = parentElementId + " " + a + " " + i
              val valueCell  = getValueCell(cellType, vElementId, v, false, 0, op)
              val attrCell   = getAttrCell(a, cellType, vElementId, valueCell, false)
              viewElement.appendChild(
                tr(
                  if (txCount % 2 == 0) cls := "even" else (),
                  if (tx != txPrev) txCell else td(),
                  if (tx != txPrev) td(txInstant) else td(),
                  attrCell,
                  valueCell(if (op) () else cls := "retracted")
                ).render
              )
              txPrev = tx
            }

            case (t, _, _, op, a, v) =>
              val op1 = if (op) "asserted" else "retracted"
            //              println(s"Ignore abandoned $a -> `$v`  $op1 in t $t")
          }

        } else {
          var attrCount = 0
          var attrPrev  = ""
          data.sortBy(t => (t._5, t._1, t._4, t._6)).foreach {
            case (t, tx, txInstant, asserted, a, v) if viewCellTypes.contains(a) => {
              i += 1
              if (a != attrPrev) {
                attrCount += 1
              }
              val txCell     = td(
                s"$t / $tx",
                cls := Rx(if (tx == curTx()) "txChosen" else "tx"),
                onmouseover := { () =>
                  curT() = t
                  curTx() = tx
                  curTxInstant() = txInstant
                }
              )
              val cellType   = viewCellTypes(a)
              val vElementId = parentElementId + " " + a + " " + i
              val valueCell  = getValueCell(cellType, vElementId, v, false, 0, asserted)
              val attrCell   = getAttrCell(a, cellType, vElementId, valueCell, false)
              viewElement.appendChild(
                tr(
                  if (attrCount % 2 == 0) cls := "even" else (),
                  txCell,
                  td(txInstant),
                  attrCell(fontWeight.bold),
                  valueCell

                ).render
              )
              txPrev = tx
              attrPrev = a
            }

            case (t, _, _, op, a, v) =>
              val op1 = if (op) "asserted" else "retracted"
            //              println(s"Ignore abandoned $a -> `$v`  $op1 in t $t")
          }
        }
      }
    }
  }
}
