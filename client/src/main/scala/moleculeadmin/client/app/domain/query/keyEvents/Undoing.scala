package moleculeadmin.client.app.domain.query.keyEvents
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.views.Base
import moleculeadmin.client.app.element.query.UndoElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.shared.api.QueryApi
import org.scalajs.dom.window
import rx.Ctx
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom.document


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
    curLastTxs.reverse.find(tx =>
      tx._5.nonEmpty && // has datoms to undo
        !undone2new.contains(tx._1) && // not already undone
        !new2undone.contains(tx._1) // not an undoing tx
    ).fold {
      window.alert("No clean txs to undo - please load more to undo further back.")
    } { cleanTx =>
      undoTxs(Seq(cleanTx._1))
    }
  }

  def undoTxs(ts: Seq[Long])(implicit ctx: Ctx.Owner): Unit = {
    queryWire().undoTxs(db, ts, enumAttrs).call().foreach {
      case Right(newTxs) =>
        // Log
        println(s"Undid ${newTxs.length} txs:")

        // Update local undone t pair caches
        ts.reverse.zip(newTxs).foreach {
          case (oldT, newTx) =>
            undone2new += oldT -> newTx._1
            new2undone += newTx._1 -> oldT

            // Show old/new t's
            println(s"  $oldT -> ${newTx._1}")
        }

        curLastTxs = curLastTxs ++ newTxs

        // Update dataTable + undo txs
        modelElements.recalc()
      case Left(err)     => window.alert(err)
    }
  }

  def populateUndoRows(err: String = "")(implicit ctx: Ctx.Owner): Unit = {
    def allNext(t: Long): Seq[Long] =
      curLastTxs.collect {
        case tx
          if tx._1 >= t && // including/after this tx
            tx._5.nonEmpty && // has datoms to undo
            !undone2new.contains(tx._1) // not already undone
        => tx._1
      }.sorted

    def cleanNext(t: Long): Seq[Long] =
      curLastTxs.collect {
        case tx
          if tx._1 >= t && // including/after this tx
            tx._5.nonEmpty && // has datoms to undo
            !undone2new.contains(tx._1) && // not already undone
            !new2undone.contains(tx._1) // not an undoing tx
        => tx._1
      }.sorted

    var countDown = curLastTxs.length
    var ePrev     = 0L
    var aPrev     = ""

    // Map t to tx/txInstant
    curLastTxs.foreach { tx =>
      t2tx = t2tx + (tx._1 -> tx._2)
      t2txInstant = t2txInstant + (tx._1 -> tx._3)
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

    var curGroupEdits      = groupEdits.filter(_._1 >= curLastTxs.head._1)
    var hasGroupEdits      = curGroupEdits.nonEmpty
    var firstT             = 0L
    var lastT              = 0L
    var isFirstOfGroupEdit = false
    var isGroupEdit        = false
    var isLastOfGroup      = false
    var isUndone           = false
    def setNextGroupEdit() =
      curGroupEdits.headOption.fold {
        firstT = 0L
        lastT = 0L
        hasGroupEdits = false
      } { pair =>
        firstT = pair._1
        lastT = pair._2
        // prepare next
        curGroupEdits = curGroupEdits.tail
        isLastOfGroup = true
      }

    setNextGroupEdit()

    curLastTxs.foreach {
      case (t, tx, txInstant, txMetaDatoms, datoms) =>
        isUndone = undone2new.contains(t)

        if (hasGroupEdits) {
          if (t == firstT) {
            isFirstOfGroupEdit = true
            isGroupEdit = true
            isLastOfGroup = false

          } else if (!isLastOfGroup && isGroupEdit && t < lastT) {
            isFirstOfGroupEdit = false

          } else if (isGroupEdit && t == lastT) {
            setNextGroupEdit()

          } else {
            isFirstOfGroupEdit = false
            isGroupEdit = false
          }
        } else {
          isFirstOfGroupEdit = false
          isGroupEdit = false
        }

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

        val isTop         = isFirstOfGroupEdit || !isGroupEdit
        val canUndo       = !(datoms.isEmpty || isUndone)
        val notLast       = countDown > 1
        val undoAllNext   = { () => undoTxs(allNext(t)) }
        val undoCleanNext = { () => undoTxs(cleanNext(t)) }
        val undoThis      = { () => undoTxs(Seq(t)) }

        datomTable1.appendChild(
          _headerRow(
            t, tx,
            isTop,
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

        datomTable1.appendChild(
          _txInstantRow(tx, txInstant, isUndone, setTx)
        )

        txMetaDatoms.foreach { case (e, a, v, _) =>
          datomTable1.appendChild(
            _txMetaDataRow(tx, e, a, v, isUndone, setTx)
          )
        }

        ePrev = 0L
        aPrev = ""
        datoms.foreach { case (e, a, v, op) =>
          val cellType        = viewCellTypes(a)
          val vElementId      = s"undoTxs $countDown $t $a"
          val showEntity      = e != ePrev
          val attr1           = if (showEntity || a != aPrev) a else ""
          val highlightEntity = { () => curEntity() = e }
          val base            = new Base
          val valueCell       = base.getValueCell(cellType, vElementId, v, true, 0, op)
          val attrCell        = base.getAttrCell(attr1, cellType, vElementId, valueCell, true)
          ePrev = e
          aPrev = a
          datomTable1.appendChild(
            _txDataRow(
              tx, e, isUndone, setTx,
              showEntity, highlightEntity, attrCell, valueCell)
          )
        }

        countDown -= 1
    }

    // Scroll to bottom
    datomTable1.scrollTop = datomTable1.scrollHeight
  }
}
