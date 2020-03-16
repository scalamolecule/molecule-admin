package moleculeadmin.client.app.domain.query.keyEvents

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.views.Base
import moleculeadmin.client.app.element.query.UndoElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.shared.api.QueryApi
import org.scalajs.dom.{document, window}
import rx.Ctx
import scala.concurrent.ExecutionContext.Implicits.global


// https://stackoverflow.com/questions/25389807/how-do-i-undo-or-reverse-a-transaction-in-datomic
// Undone txs can't be re-undone
// Undoing txs are not considered clean anymore. One can choose whether to undo those or not

trait Undoing extends UndoElements with QueryApi {

  type keepBooPickleImport_Undoing = PickleState

  // Local cache (reset on each refresh)
  var curFirstT      = 0L
  var t2tx           = Map.empty[Long, Long]
  var t2txInstant    = Map.empty[Long, String]
  var cleanMouseover = true

  def toggleUndo()(implicit ctx: Ctx.Owner): Unit = {
    showUndo() = !showUndo.now
  }

  def undoLastClean(implicit ctx: Ctx.Owner): Unit = {
    curLastTxResults.reverse.find(txResult =>
      txResult._5.nonEmpty && // has datoms to undo
        !undone2new.contains(txResult._1) && // not already undone
        !new2undone.contains(txResult._1) // not an undoing tx
    ).fold {
      window.alert("No clean txs to undo - please load more to undo further back.")
    } { cleanTx =>
      undoTxs(Seq(cleanTx._1))
    }
  }

  def undoTxs(ts: Seq[Long])(implicit ctx: Ctx.Owner): Unit = {
    queryWire().undoTxs(db, ts, enumAttrs).call().foreach {
      case Right(newTxResults) =>
        // Log
        println(s"Undid ${newTxResults.length} txs:")

        // Update local undone t pair caches
        ts.reverse.zip(newTxResults).foreach {
          case (oldT, newTx) =>
            undone2new += oldT -> newTx._1
            new2undone += newTx._1 -> oldT

            // Show old/new t's
            println(s"  $oldT -> ${newTx._1}")
        }

        curLastTxResults = curLastTxResults ++ newTxResults

        // Update dataTable + undo txs
        modelElements.recalc()

      case Left(err) => window.alert(err)
    }
  }

  def populateUndoRows(err: String = "")(implicit ctx: Ctx.Owner): Unit = {
    def allNext(t: Long): Seq[Long] =
      curLastTxResults.collect {
        case txResult
          if txResult._1 >= t && // including/after this tx
            txResult._5.nonEmpty && // has datoms to undo
            !undone2new.contains(txResult._1) // not already undone
        => txResult._1
      }.sorted

    def cleanNext(t: Long): Seq[Long] =
      curLastTxResults.collect {
        case txResult
          if txResult._1 >= t && // including/after this tx
            txResult._5.nonEmpty && // has datoms to undo
            !undone2new.contains(txResult._1) && // not already undone
            !new2undone.contains(txResult._1) // not an undoing tx
        => txResult._1
      }.sorted

    var countDown = curLastTxResults.length
    var ePrev     = 0L
    var aPrev     = ""

    // Map t to tx/txInstant
    curLastTxResults.foreach { txResult =>
      t2tx = t2tx + (txResult._1 -> txResult._2)
      t2txInstant = t2txInstant + (txResult._1 -> txResult._3)
    }

    // Fill datomTable
    val datomTable1 =
      if (datomTable == null)
        document.getElementById("undoTxs")
      else
        datomTable

    datomTable1.innerHTML = ""
    datomTable1.appendChild(
      _loadMoreRow(err, { () =>
        println("load more...")
        showUndo.recalc()
      })
    )

    curLastTxResults.foreach {
      case (t, tx, txInstant, txMetaDatoms, dataDatoms) =>
        val setTx            = { () =>
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

        val isUndone      = undone2new.contains(t)
        val canUndo       = !(dataDatoms.isEmpty || isUndone)
        val notLast       = countDown > 1
        val undoAllNext   = { () => undoTxs(allNext(t)) }
        val undoCleanNext = { () => undoTxs(cleanNext(t)) }
        val undoThis      = { () => undoTxs(Seq(t)) }

        datomTable1.appendChild(
          _headerRow(
            t, tx,
            isUndone,
            canUndo,
            notLast,
            setTx,
            highlightUndoneT,
            highlightNewT,
            undoAllNext,
            undoCleanNext,
            undoThis
          )
        )

        txMetaDatoms.foreach { case (e, a, v, _) =>
          datomTable1.appendChild(
            _txMetaDataRow(tx, e, a, v, isUndone, setTx)
          )
        }

        val maxVisibleEntities = 10
        ePrev = 0L
        aPrev = ""
        var visible = true
        var eCount  = 0
        var i       = 0
        dataDatoms.foreach { case (e, a, v, op) =>
          i += 1
          if (visible) {
            val showEntity = e != ePrev
            if (showEntity && eCount == maxVisibleEntities) {
              val more = dataDatoms.length - i + 1
              datomTable1.appendChild(
                _txDataMoreRow(tx, isUndone,
                  more + " more datoms in tx... (see all in Transaction view)"
                )
              )
              visible = false
            } else {
              if (showEntity) eCount += 1
              val cellType        = viewCellTypes(a)
              val vElementId      = s"undoTxs $countDown $t $i $a"
              val attr1           = if (showEntity || a != aPrev) a else ""
              val highlightEntity = { () => if (!curEntityLocked.now) curEntity() = e }
              val base            = new Base
              val valueCell       = base.getValueCell(cellType, vElementId, v, false, 0, op)
              val attrCell        = base.getAttrCell(attr1, cellType, vElementId, valueCell, false)
              ePrev = e
              aPrev = a
              datomTable1.appendChild(
                _txDataRow(
                  tx, e, isUndone, setTx,
                  showEntity, highlightEntity, attrCell, valueCell)
              )
            }
          }
        }

        countDown -= 1
    }

    // Scroll to bottom
    datomTable1.scrollTop = datomTable1.scrollHeight
  }
}
