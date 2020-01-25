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
  var chunks         = 1
  var t2tx           = Map.empty[Long, Long]
  var t2txInstant    = Map.empty[Long, String]
  var cleanMouseover = true
  var zed            = Seq.empty[Long]

  def toggleUndo()(implicit ctx: Ctx.Owner): Unit = {
    showUndo() = !showUndo.now
  }

  def undoLastClean(chunks: Int)(implicit ctx: Ctx.Owner): Unit = {
    println("undoLastClean")

    // Avoid infinite recursion
    if (chunks == 4)
      throw new RuntimeException(
        "Can't search for more than 3 chunks of tx data")

    queryWire().getLastTxs(db, chunks, enumAttrs).call().foreach {
      case Right(txs) =>

        println(undone2new)
        println(new2undone)

        txs.reverse.find(tx =>
          tx._5.nonEmpty && // has datoms to undo
            !undone2new.contains(tx._1) && // not already undone
            !zed.contains(tx._1)
        ).fold(
          // Recursively retry with one more chunk
          undoLastClean(chunks + 1)
        ) { cleanTx =>

          println("--- " + cleanTx._1)
          txs.foreach(tx => println(tx._1))

          undoTxs(txs, Seq(cleanTx._1))

          // avoid cmd-z the same txs (within a session - resets on refresh)
          zed = zed :+ cleanTx._1
        }

      case Left(err) =>
        throw new RuntimeException("Error undoing last tx: " + err)
    }
  }

  def undoTxs(
    txs: Array[TxData],
    ts: Seq[Long]
  )(implicit ctx: Ctx.Owner): Unit = {
    queryWire().undoTxs(db, ts, enumAttrs).call().foreach {
      case Right(newTxs) =>
        // Log
        println(s"Undid ${ts.length} txs:")

        // Update local undone t pair caches
        ts.reverse.zip(newTxs).foreach {
          case (oldT, newTx) =>
            undone2new += oldT -> newTx._1
            new2undone += newTx._1 -> oldT

            // Show old/new t's
            println(s"  $oldT -> ${newTx._1}")
        }

        if (showUndo.now) {
          // Render undo
          setUndoRows(txs ++ newTxs)
        }

        // Update dataTable
        modelElements.recalc()
      case Left(err)     => window.alert(err)
    }
  }


  def setUndoRows(
    txs: Array[TxData],
    err: String = ""
  )(implicit ctx: Ctx.Owner): Unit = {
    def allNext(t: Long): Seq[Long] =
      txs.collect {
        case tx
          if tx._1 >= t && // including/after this tx
            tx._5.nonEmpty && // has datoms to undo
            !undone2new.contains(tx._1) // not already undone
        => tx._1
      }.sorted

    def cleanNext(t: Long): Seq[Long] =
      txs.collect {
        case tx
          if tx._1 >= t && // including/after this tx
            tx._5.nonEmpty && // has datoms to undo
            !undone2new.contains(tx._1) && // not already undone
            !new2undone.contains(tx._1) // not an undoing tx
        => tx._1
      }.sorted

    var countDown = txs.length
    var ePrev     = 0L
    var aPrev     = ""

    // Map t to tx/txInstant
    txs.foreach { tx =>
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
        chunks += 1
        showUndo.recalc()
      })
    )

    var curGroupEdits      = groupEdits.filter(_._1 >= txs.head._1)
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

    txs.foreach {
      case (t, tx, txInstant, txMetaDatoms, datoms) =>
        println(s"--- $t")
        isUndone = undone2new.contains(t)

        if (hasGroupEdits) {
          if (t == firstT) {
            println("first")
            isFirstOfGroupEdit = true
            isGroupEdit = true
            isLastOfGroup = false

          } else if (!isLastOfGroup && isGroupEdit && t < lastT) {
            println("...")
            isFirstOfGroupEdit = false

          } else if (isGroupEdit && t == lastT) {
            println("last")
            setNextGroupEdit()

          } else {
            println("not")
            isFirstOfGroupEdit = false
            isGroupEdit = false
          }
        } else {
          println("hasGroupEdits not")
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
        val undoAllNext   = { () => undoTxs(txs, allNext(t)) }
        val undoCleanNext = { () => undoTxs(txs, cleanNext(t)) }
        val undoThis      = { () => undoTxs(txs, Seq(t)) }

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
