package moleculeadmin.client.app.domain.query.data.update
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.TypeValidation
import moleculeadmin.client.app.domain.query.keyEvents.Editing
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{TableCell, TableRow}
import org.scalajs.dom.raw.NodeList
import rx.Ctx

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
  extends BodyElements with TypeValidation with ColOps with Editing {

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

    // Update table row(s)

    val (tString, txString) = (t.toString, tx.toString)

    val tLambda = if (tIndex > 0) {
      rowCells: NodeList => {
        val tCell = rowCells.item(tIndex + 1).asInstanceOf[TableCell]
        tCell.textContent = tString
        tCell.className = "txChosen"
      }
    } else (_: NodeList) => ()

    val txLambda = if (tIndex > 0) {
      rowCells: NodeList => {
        val txCell = rowCells.item(txIndex + 1).asInstanceOf[TableCell]
        txCell.textContent = txString
        txCell.className = "txChosen"
      }
    } else (_: NodeList) => ()

    val txInstantLambda = if (tIndex > 0) {
      rowCells: NodeList => {
        val txInstantCell = rowCells.item(txInstantIndex + 1).asInstanceOf[TableCell]
        txInstantCell.textContent = txInstant
        txInstantCell.className = "txChosen"
      }
    } else (_: NodeList) => ()


    val updateTxCells: NodeList => Unit = {
      (tArray, txArray, txInstantArray) match {
        case (None, None, None) => (_: NodeList) => ()
        case (_, None, None)    => (rowCells: NodeList) =>
          tLambda(rowCells)
        case (None, _, None)    => (rowCells: NodeList) =>
          txLambda(rowCells)
        case (None, None, _)    => (rowCells: NodeList) =>
          txInstantLambda(rowCells)
        case (_, None, _)       => (rowCells: NodeList) =>
          tLambda(rowCells)
          txInstantLambda(rowCells)
        case (None, _, _)       => (rowCells: NodeList) =>
          txLambda(rowCells)
          txInstantLambda(rowCells)
        case (_, _, None)       => (rowCells: NodeList) =>
          tLambda(rowCells)
          txLambda(rowCells)
        case (_, _, _)          => (rowCells: NodeList) =>
          tLambda(rowCells)
          txLambda(rowCells)
          txInstantLambda(rowCells)
      }
    }

    if (valueColIndex == -1) {
      if (related == 0) {
        // Only one row updated
        updateTxCells(cell.parentNode.childNodes)
      } else {
        val tableRows = row.parentNode.childNodes
        while (i < tableRows.length) {
          val rowCells = tableRows.item(i).childNodes
          // Update all rows matching current eid
          if (rowCells.item(eIndex + 1).textContent == s"$eid") {
            updateTxCells(rowCells)
          }
          i += 1
        }
      }
    } else {
      val tableRows = row.parentNode.childNodes
      affectedRows.foreach { rowIndex =>
        val rowCells = tableRows.item(rowIndex).childNodes
        updateTxCells(rowCells)
      }
    }


    // Update arrays in client memory

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


    // Update current tx data
    curT() = t
    curTx() = tx
    curTxInstant() = txInstant

    // Force marking the entity changed
    curEntity() = eid

    // don't know why we need to poke curEntity twice to redraw Entity view...
    curEntity.recalc()
    entityHistorySort.recalc()
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
