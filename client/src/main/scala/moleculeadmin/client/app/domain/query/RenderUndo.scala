package moleculeadmin.client.app.domain.query

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.views.Base
import moleculeadmin.client.app.element.query.{GroupedAttrElements, UndoElements}
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.html.Element
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global


case class RenderUndo()(implicit ctx: Ctx.Owner)
  extends Base() with UndoElements {

  type keepBooPickleImport_RenderUndo = PickleState

  var t2tx           = Map.empty[Long, Long]
  var t2txInstant    = Map.empty[Long, String]
  var cleanMouseover = true


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    showUndo()
    if (showUndo.now) {
      queryWire().getLastTxs(db, 1, enumAttrs).call().foreach {
        case Right(txData) =>
          txData.foreach { txD =>
            t2tx = t2tx + (txD._1 -> txD._2)
            t2txInstant = t2txInstant + (txD._1 -> txD._3)
          }
          setUndoRows(txData)

        case Left(err) =>
          setUndoRows(Array.empty[TxData], err)
      }
      container
    }
    else {
      span()
    }
  }

  def undoTxs(
    txs: Array[TxData],
    ts: Seq[Long]
  ): Unit = {
    val tsNotUndone = ts.diff(undone2new.keySet.toList).sorted
    queryWire().undoTxs(db, tsNotUndone, enumAttrs).call().foreach {
      case Right(newTxs) =>
        // Update local undone t pair caches
        tsNotUndone.reverse.zip(newTxs).foreach {
          case (t, newTx) =>
            undone2new = undone2new + (t -> newTx._1)
            new2undone = new2undone + (newTx._1 -> t)
        }

        // Render undo
        setUndoRows(txs ++ newTxs)

        // Update dataTable
        modelElements.recalc()
      case Left(err)     => window.alert(err)
    }

  }


  def setUndoRows(txs: Array[TxData], err: String = ""): Unit = {
    def from(t: Long): Seq[Long] = {
      txs.collect {
        case tx if tx._1 >= t && tx._5.nonEmpty => tx._1
      }
    }

    var countDown = txs.length
    var ePrev     = 0L
    var aPrev     = ""


    val loadMore = { () => println("todo...") }


    // Fill datomTable
    datomTable.innerHTML = ""
    datomTable.appendChild(
      _loadMoreRow(err, loadMore)
    )

    txs.foreach {
      case (t, tx, txInstant, txMetaDatoms, datoms) =>
        ePrev = 0L
        aPrev = ""
        val setTx = { () =>
          if (cleanMouseover) {
            curT() = t
            curTx() = tx
            curTxInstant() = txInstant
          } else {
            cleanMouseover = true
          }
        }

        val highlightUndoneT = { () =>
          new2undone.get(t).fold(()) { undoneT =>
            cleanMouseover = false
            curT() = undoneT
            t2tx.get(undoneT).fold(())(curTx() = _)
            t2txInstant.get(undoneT).fold(())(curTxInstant() = _)
          }
        }
        val highlightNewT    = { () =>
          undone2new.get(t).fold(()) { newT =>
            cleanMouseover = false
            curT() = newT
            t2tx.get(newT).fold(())(curTx() = _)
            t2txInstant.get(newT).fold(())(curTxInstant() = _)
          }
        }
        val canUndo          = !(datoms.isEmpty || undone2new.contains(t))
        val notLast          = countDown > 1
        val undoFollowing    = { () => undoTxs(txs, from(t)) }
        val undoThis         = { () => undoTxs(txs, Seq(t)) }

        datomTable.appendChild(
          _headerRow(
            t, tx,
            setTx,
            highlightUndoneT,
            highlightNewT,
            canUndo,
            notLast,
            undoFollowing,
            undoThis
          )
        )

        datomTable.appendChild(
          _txInstantRow(tx, txInstant, setTx)
        )

        txMetaDatoms.foreach { case (e, a, v, _) =>
          datomTable.appendChild(
            _txMetaDataRow(tx, e, a, v, setTx)
          )
        }

        datoms.foreach { case (e, a, v, op) =>
          val cellType        = viewCellTypes(a)
          val vElementId      = s"undoTxs $countDown $t $a"
          val showEntity      = e != ePrev
          val attr1           = if (showEntity || a != aPrev) a else ""
          val highlightEntity = { () => curEntity() = e }
          val valueCell       = getValueCell(cellType, vElementId, v, true, 0, op)
          val attrCell        = getAttrCell(attr1, cellType, vElementId, valueCell, true)
          ePrev = e
          aPrev = a
          datomTable.appendChild(
            _txDataRow(
              tx, e, a, v, setTx,
              showEntity, highlightEntity, attrCell, valueCell)
          )
        }

        countDown -= 1
    }

    // Scroll to bottom
    datomTable.scrollTop = datomTable.scrollHeight
  }
}
