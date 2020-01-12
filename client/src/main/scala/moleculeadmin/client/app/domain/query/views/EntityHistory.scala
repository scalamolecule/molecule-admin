package moleculeadmin.client.app.domain.query.views
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.document
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class EntityHistory()(implicit ctx: Ctx.Owner) extends Base() {
  type keepBooPickleImport_EntityHistory = PickleState

  def view: Rx.Dynamic[TypedTag[Element]] = Rx {
    (curEntity(), entityHistorySort()) match {
      case (0, _) => // no entity id marked yet

      case (eid, sort) if curViews.now.contains("view04_EntityHistory") =>
        val view = document.getElementById("entityHistoryEid")
        if (view == null) {
          // Start fresh
          curEntity() = 0
        } else {
          view.innerHTML = ""

          val byTx = if (sort == "tx")
            span("tx")
          else
            a(href := "#", "tx", onclick := { () =>
              entityHistorySort() = "tx"
            })

          val byAttr = if (sort == "attr")
            span("attr", paddingRight := 20)
          else
            a(href := "#", "attr", paddingRight := 20, onclick := { () =>
              entityHistorySort() = "attr"
            })

          view.appendChild(
            span(byTx, " | ", byAttr, eid.toString).render
          )
          addEntityHistoryRows("entityHistoryViewTable", eid, sort)
        }

      case _ => // don't update non-present entityView
    }
    _entityHistoryView("Point on entity id...")
  }


  def addEntityHistoryRows(parentElementId: String, eid: Long, sort: String): Unit = {
    val viewElement = document.getElementById(parentElementId)
    if (viewElement != null) {
      queryWire().getEntityHistory(db, eid, enumAttrs).call().foreach { data =>
        viewElement.innerHTML = ""
        var i      = 0
        var txPrev = 0L

        if (sort == "tx") {
          var txCur   = 0L
          var txCount = 0
          data.sortBy(t => (t._1, t._5, t._4, t._6)).foreach {
            case (t, tx, txInstant, asserted, attr, v) => {
              i += 1
              if (tx != txPrev) {
                txCur = tx
                txCount += 1
              }
              val cellType   = viewCellTypes(attr)
              val vElementId = parentElementId + " " + attr + " " + i

              val txCell    = td(
//                s"$t / $tx",
                t,
                cls := Rx(if (tx == curTx()) "txChosen" else "tx"),
                onmouseover := { () =>
                  curT() = t
                  curTx() = tx
                  curTxInstant() = txInstant
                }
              )
              val valueCell = getValueCell(cellType, vElementId, v, true, 0, asserted)
              val attrCell  = getAttrCell(attr, cellType, vElementId, valueCell, true)
              viewElement.appendChild(
                tr(
                  if (txCount % 2 == 0)
                    cls := "even"
                  else
                    (),
                  if (tx != txPrev) txCell else td(),
                  if (tx != txPrev) td(txInstant) else td(),
                  attrCell,
                  valueCell(if (asserted) () else cls := "retracted")
                ).render
              )
              txPrev = tx
            }
          }

        } else {
          var attrCur   = ""
          var attrCount = 0
          var attrPrev  = ""
          data.sortBy(t => (t._5, t._1, t._4, t._6)).foreach {
            case (t, tx, txInstant, asserted, attr, v) => {
              i += 1
              if (attr != attrPrev) {
                attrCur = attr
                attrCount += 1
              }
              val cellType   = viewCellTypes(attr)
              val vElementId = parentElementId + " " + attr + " " + i
              val txCell     = td(
//                s"$t / $tx",
                t,
                cls := Rx(if (tx == curTx()) "txChosen" else "tx"),
                onmouseover := { () =>
                  curT() = t
                  curTx() = tx
                  curTxInstant() = txInstant
                }
              )
              val valueCell  = getValueCell(cellType, vElementId, v, true, 0, asserted)
              val attrCell   = getAttrCell(attr, cellType, vElementId, valueCell, true)
              viewElement.appendChild(
                tr(
                  if (attrCount % 2 == 0)
                    cls := "even"
                  else
                    (),
                  txCell,
                  td(txInstant),
                  attrCell(fontWeight.bold),
                  valueCell

                ).render
              )
              txPrev = tx
              attrPrev = attr
            }
          }
        }
      }
    }
  }
}
