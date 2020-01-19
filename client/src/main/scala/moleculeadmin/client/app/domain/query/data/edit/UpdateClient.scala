package moleculeadmin.client.app.domain.query.data.edit
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.TypeValidation
import moleculeadmin.client.app.domain.query.keyEvents.Editing
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{TableCell, TableRow}
import rx.Ctx

abstract class UpdateClient[T](
  cols: Seq[Col],
  qr: QueryResult,
  origArray: Array[Option[T]],
  editArray: Array[Option[T]],
  baseClass: String,
  rowIndex: Int,
  arrayIndex: Int,
  colIndex: Int,
  related: Int,
  nsAlias: String,
  nsFull: String,
  attr: String,
  enums: Seq[String]
)(implicit ctx: Ctx.Owner)
  extends TxLambdas(cols, qr) with TypeValidation with ColOps with Editing {

  var i          = 0
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
    cellId: String,
    cell: TableCell,
    row: TableRow,
    eid: Long,
    oldVOpt: Option[T],
    isNum: Boolean): Unit

  def updateClient(
    t: Long, tx: Long, txInstant: String,
    cell: TableCell,
    row: TableRow,
    eid: Long,
    newVopt: Option[T],
    oldVopt: Option[T] = None,
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
        i = 0
        while (i < eArray.length) {
          eArray(i) match {
            case Some(`eid`) => updateArrays(i)
            case _           =>
          }
          i += 1
        }
      }
    } else {
      i = 0
      while (i < affectedIndexes.length) {
        updateArrays(affectedIndexes(i))
        i += 1
      }
    }

    // Update table row(s) ---------------------------------------

    val highlightT = (curRow: TableRow) => {
      // Update callback with new t
      val newCell = tLambda(
        qr.arrayIndexes(tIndex), tIndex)(ctx)(rowIndex).render
      val oldCell = curRow.childNodes.item(tIndex + 1)
      curRow.replaceChild(newCell, oldCell)
    }

    val highlightTx = (curRow: TableRow) => {
      // Update callback with new tx
      val newCell = txLambda(
        qr.arrayIndexes(txIndex), txIndex)(ctx)(rowIndex).render
      val oldCell = curRow.childNodes.item(txIndex + 1)
      curRow.replaceChild(newCell, oldCell)
    }

    val highlightTxInstant = (curRow: TableRow) => {
      // Update callback with new txInstant
      val newCell = txInstantLambda(
        qr.arrayIndexes(txInstantIndex), txInstantIndex)(ctx)(rowIndex).render
      val oldCell = curRow.childNodes.item(txInstantIndex + 1)
      curRow.replaceChild(newCell, oldCell)
    }


    val highlightTxCells: TableRow => Unit = {
      (tArray, txArray, txInstantArray) match {
        case (None, None, None) => (_: TableRow) => ()
        case (_, None, None)    => (curRow: TableRow) =>
          highlightT(curRow)
        case (None, _, None)    => (curRow: TableRow) =>
          highlightTx(curRow)
        case (None, None, _)    => (curRow: TableRow) =>
          highlightTxInstant(curRow)
        case (_, None, _)       => (curRow: TableRow) =>
          highlightT(curRow)
          highlightTxInstant(curRow)
        case (None, _, _)       => (curRow: TableRow) =>
          highlightTx(curRow)
          highlightTxInstant(curRow)
        case (_, _, None)       => (curRow: TableRow) =>
          highlightT(curRow)
          highlightTx(curRow)
        case (_, _, _)          => (curRow: TableRow) =>
          highlightT(curRow)
          highlightTx(curRow)
          highlightTxInstant(curRow)
      }
    }

    if (valueColIndex == -1) {
      if (related == 0) {
        // Only one row updated
        highlightTxCells(row)
      } else {
        val tableRows = row.parentNode.childNodes
        while (i < tableRows.length) {
          val curRow = tableRows.item(i)
          // Update all rows matching current eid
          if (curRow.childNodes.item(eIndex + 1).textContent == s"$eid") {
            highlightTxCells(curRow.asInstanceOf[TableRow])
          }
          i += 1
        }
      }
    } else {
      val tableRows = row.parentNode.childNodes
      affectedRows.foreach { rowIndex =>
        highlightTxCells(tableRows.item(rowIndex).asInstanceOf[TableRow])
      }
    }


    // Update current tx data ---------------------------------------

    curT() = t
    curTx() = tx
    curTxInstant() = txInstant

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
