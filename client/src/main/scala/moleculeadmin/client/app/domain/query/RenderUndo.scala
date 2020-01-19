package moleculeadmin.client.app.domain.query

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.grouped.Grouped
import moleculeadmin.client.app.domain.query.views.Base
import moleculeadmin.client.app.element.query.GroupedAttrElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.document
import org.scalajs.dom.html.{Element, Table}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom.{document, window}
import scala.scalajs.js


case class RenderUndo()(implicit ctx: Ctx.Owner)
  extends Base() with GroupedAttrElements {
  type keepBooPickleImport_RenderUndo = PickleState

  val datomTable = table(
    cls := "undoTxs",
    id := "undoTxs"
  ).render


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    showUndo()
    // Update after edit
    curEntity()
    if (showUndo.now) {
      queryWire().getLastTxs(db, 1, enumAttrs).call().foreach(addRows)
      _cardsContainer(
        _card(
          _cardHeader(h5("Undo")),
          _cardBody(
            padding := 0,
            datomTable
          )
        )
      )
    }
    else {
      span()
    }
  }

  def addRows(txs: Array[TxData]): Unit = {
    datomTable.innerHTML = ""
    datomTable.appendChild(
      tr(
        td(
          colspan := 3,
          paddingBottom := 5,
          a(href := "#", "Load more...")
        )
      ).render
    )

    var i     = txs.length
    var ePrev = 0L
    var aPrev = ""

    txs.foreach {
      case (t, tx, txInstant, txMetaDatoms, datoms) =>
        ePrev = 0L
        aPrev = ""
        val setTx = { () =>
          curT() = t
          curTx() = tx
          curTxInstant() = txInstant
        }

        // t -------------------------------------------------------------------

        datomTable.appendChild(
          tr(
            onmouseover := setTx,
            td(
              cls := Rx(if (tx == curTx()) "header chosen" else "header"),
              t
            ),
            td(
              colspan := 2,
              cls := Rx(if (tx == curTx()) "header chosen" else "header"),
              textAlign.right,

              if (i > 1) a(
                href := "#",
//                s"Undo last $i txs",
                s"Undo this and following txs",
                onclick := { () => }
              ) else (),
              a(
                href := "#",
                marginLeft := 15,
                "Undo this tx",
                onclick := { () => }
              ),
            )
          ).render
        )


        // txInstant -----------------------------------------------------------

        datomTable.appendChild(
          tr(
            cls := Rx(if (tx == curTx()) "txMetaData chosen" else "txMetaData"),
            onmouseover := setTx,
            td(tx),
            td(":db/txInstant"),
            td(txInstant),
          ).render
        )


        // tx meta data --------------------------------------------------------

        txMetaDatoms.foreach {
          case (e, a, v, _) => {
            datomTable.appendChild(
              tr(
                cls := Rx(if (tx == curTx()) "txMetaData chosen" else "txMetaData"),
                onmouseover := setTx,
                td(e),
                td(a),
                td(v),
              ).render
            )
          }
        }


        // tx data -------------------------------------------------------------

        datoms.foreach {
          case (e, a, v, op) => {
            val entityCell =
              if (e != ePrev)
                td(
                  e,
                  cls := Rx(if (e == curEntity()) "eid chosen" else "eid"),
                  onmouseover := { () => curEntity() = e }
                )
              else
                td()

            val cellType   = viewCellTypes(a)
            val vElementId = s"undoTxs $i $t $a"
            val attr1      = if (e != ePrev || a != aPrev) a else ""
            val valueCell  = getValueCell(cellType, vElementId, v, true, 0, op)
            val attrCell   = getAttrCell(attr1, cellType, vElementId, valueCell, true)

            datomTable.appendChild(
              tr(
                cls := Rx(if (tx == curTx()) "chosen" else ""),
                onmouseover := setTx,
                entityCell,
                attrCell,
                valueCell
              ).render
            )
            ePrev = e
            aPrev = a
          }
        }

        i -= 1
    }

    // Scroll to bottom
    datomTable.scrollTop = datomTable.scrollHeight
  }
}
