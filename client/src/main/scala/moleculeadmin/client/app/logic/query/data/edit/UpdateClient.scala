package moleculeadmin.client.app.logic.query.data.edit
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.TypeValidation
import moleculeadmin.client.app.logic.query.keyEvents.Editing
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{TableCell, TableRow}
import rx.Ctx
import scalatags.JsDom.all.s

abstract class UpdateClient[T](
  cols: Seq[Col],
  qr: QueryResult,
  origArray: Array[Option[T]],
  editArray: Array[Option[T]],
  baseClass: String,
  rowIndex: Int,
  related: Int,
  nsAlias: String,
  nsFull: String,
  attr: String,
  enums: Seq[String]
)(implicit ctx: Ctx.Owner)
  extends TxLambdas(cols, qr) with TypeValidation with ColOps with Editing {

  val attrFull   = s":$nsFull/${clean(attr)}"
  val enumPrefix = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"

  // Tx update coordinates
  val (
    eIndex,
    tArray, tIndex,
    txArray, txIndex,
    txInstantArray, txInstantIndex
    ) = getTxArrays(cols, qr, nsAlias, nsFull, attr)

  // Interface for all cardinalities
  def update(
    cellIdMaker: Int => String,
    cellId: String,
    cell: TableCell,
    row: TableRow,
    eid: Long,
    oldVOpt: Option[T],
    isNum: Boolean): Unit

  def updateClient(
    cellIdMaker: Int => String,
    t: Long, tx: Long, txInstant: String,
    row: TableRow,
    eid: Long,
    newVopt: Option[T],
    valueColIndex: Int = -1,
    affectedRows: List[Int] = Nil,
    affectedIndexes: Array[Int] = Array.empty[Int]
  ): Unit = {

    // Update arrays in client memory ---------------------------------------

    val updateArrays = {
      val (tDouble, txDouble) = (t.toDouble, tx.toDouble)
      (tArray, txArray, txInstantArray) match {
        case (None, None, None)                                     =>
          (rowIndex: Int) => {
            editArray(rowIndex) = newVopt
          }
        case (Some(tArray1), None, None)                            =>
          (rowIndex: Int) => {
            editArray(rowIndex) = newVopt
            tArray1(rowIndex) = Some(tDouble)
          }
        case (None, Some(txArray1), None)                           =>
          (rowIndex: Int) => {
            editArray(rowIndex) = newVopt
            txArray1(rowIndex) = Some(txDouble)
          }
        case (None, None, Some(txInstantArray1))                    =>
          (rowIndex: Int) => {
            editArray(rowIndex) = newVopt
            txInstantArray1(rowIndex) = Some(txInstant)
          }
        case (Some(tArray1), None, Some(txInstantArray1))           =>
          (rowIndex: Int) => {
            editArray(rowIndex) = newVopt
            tArray1(rowIndex) = Some(tDouble)
            txInstantArray1(rowIndex) = Some(txInstant)
          }
        case (None, Some(txArray1), Some(txInstantArray1))          =>
          (rowIndex: Int) => {
            editArray(rowIndex) = newVopt
            txArray1(rowIndex) = Some(txDouble)
            txInstantArray1(rowIndex) = Some(txInstant)
          }
        case (Some(tArray1), Some(txArray1), None)                  =>
          (rowIndex: Int) => {
            editArray(rowIndex) = newVopt
            tArray1(rowIndex) = Some(tDouble)
            txArray1(rowIndex) = Some(txDouble)
          }
        case (Some(tArray1), Some(txArray1), Some(txInstantArray1)) =>
          (rowIndex: Int) => {
            editArray(rowIndex) = newVopt
            tArray1(rowIndex) = Some(tDouble)
            txArray1(rowIndex) = Some(txDouble)
            txInstantArray1(rowIndex) = Some(txInstant)
          }
      }
    }

    if (valueColIndex == -1) {
      if (related == 0) {
        // Update this row only
        updateArrays(rowIndex)
      } else {
        // Update all rows with this eid
        val eArray = qr.num(qr.arrayIndexes(eIndex))
        var i      = 0
        val length = eArray.length
        while (i < length) {
          eArray(i) match {
            case Some(`eid`) => updateArrays(i)
            case _           =>
          }
          i += 1
        }
      }
    } else {
      var i      = 0
      val length = affectedIndexes.length
      while (i < length) {
        updateArrays(affectedIndexes(i))
        i += 1
      }
    }

    // Replace table tx cells ---------------------------------------

    val replaceT = (rowIndex1: Int, curRow: TableRow) => {
      // Update callback with new t
      val newCell = tLambda(
        cellIdMaker,
        qr.arrayIndexes(tIndex),
        tIndex
      )(ctx)(rowIndex1).render
      val oldCell = curRow.childNodes.item(tIndex + 1)
      curRow.replaceChild(newCell, oldCell)
    }

    val replaceTx = (rowIndex1: Int, curRow: TableRow) => {
      // Update callback with new tx
      val newCell = txLambda(
        cellIdMaker,
        qr.arrayIndexes(txIndex),
        txIndex
      )(ctx)(rowIndex1).render
      val oldCell = curRow.childNodes.item(txIndex + 1)
      curRow.replaceChild(newCell, oldCell)
    }

    val replaceTxInstant = (rowIndex1: Int, curRow: TableRow) => {
      // Update callback with new txInstant
      val newCell = txInstantLambda(
        cellIdMaker,
        qr.arrayIndexes(txInstantIndex),
        txInstantIndex
      )(ctx)(rowIndex1).render
      val oldCell = curRow.childNodes.item(txInstantIndex + 1)
      curRow.replaceChild(newCell, oldCell)
    }


    val replaceTxCells: (Int, TableRow) => Unit = {
      (tArray, txArray, txInstantArray) match {
        case (None, None, None) =>
          (_: Int, _: TableRow) => ()

        case (_, None, None) =>
          (rowIndex1: Int, curRow: TableRow) =>
            replaceT(rowIndex1, curRow)

        case (None, _, None) =>
          (rowIndex1: Int, curRow: TableRow) =>
            replaceTx(rowIndex1, curRow)

        case (None, None, _) =>
          (rowIndex1: Int, curRow: TableRow) =>
            replaceTxInstant(rowIndex1, curRow)

        case (_, None, _) =>
          (rowIndex1: Int, curRow: TableRow) =>
            replaceT(rowIndex1, curRow)
            replaceTxInstant(rowIndex1, curRow)

        case (None, _, _) =>
          (rowIndex1: Int, curRow: TableRow) =>
            replaceTx(rowIndex1, curRow)
            replaceTxInstant(rowIndex1, curRow)

        case (_, _, None) =>
          (rowIndex1: Int, curRow: TableRow) =>
            replaceT(rowIndex1, curRow)
            replaceTx(rowIndex1, curRow)

        case (_, _, _) =>
          (rowIndex1: Int, curRow: TableRow) =>
            replaceT(rowIndex1, curRow)
            replaceTx(rowIndex1, curRow)
            replaceTxInstant(rowIndex1, curRow)
      }
    }

    if (valueColIndex == -1) {
      if (related == 0) {
        // Only one row updated
        replaceTxCells(rowIndex, row)
      } else {
        val tableRows = row.parentNode.childNodes
        var rowIndex  = 0
        val length    = tableRows.length
        while (rowIndex < length) {
          val curRow = tableRows.item(rowIndex)
          // Update all rows matching current eid
          if (curRow.childNodes.item(eIndex + 1).textContent == s"$eid") {
            replaceTxCells(rowIndex, curRow.asInstanceOf[TableRow])
          }
          rowIndex += 1
        }
      }
    } else {
      val tableRows = row.parentNode.childNodes
      affectedRows.foreach { rowIndex =>
        replaceTxCells(rowIndex, tableRows.item(rowIndex).asInstanceOf[TableRow])
      }
    }


    // Update current tx data ---------------------------------------

    curT() = t
    curTx() = tx
    curTxInstant() = txInstant

    // Re-calculate undo
    showUndo.recalc()

    // Re-calculate grouped
    groupedColIndexes.recalc()

    // Show entity and make sure it is recalculated if it was already chosen
    curEntity() = eid
    curEntity.recalc()
  }

  def setCellEditMode(cell: TableCell, newVopt: Option[T]): Unit = {
    val oldVopt = origArray(rowIndex)
    if (oldVopt == newVopt)
      cell.className = baseClass
    else
      newVopt match {
        case None                       => cell.className = s"$baseClass retract"
        case Some(_) if oldVopt.isEmpty => cell.className = s"$baseClass assert"
        case Some(_)                    => cell.className = s"$baseClass update"
      }
  }
}
