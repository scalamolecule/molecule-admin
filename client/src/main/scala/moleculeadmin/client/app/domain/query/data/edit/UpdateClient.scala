package moleculeadmin.client.app.domain.query.data.edit
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import org.scalajs.dom.NodeList
import org.scalajs.dom.html.{TableCell, TableRow}
import rx.Ctx
import scalatags.JsDom.all.s

abstract class UpdateClient[T](db: String,
                               cols: Seq[Col],
                               qr: QueryResult,
                               origArray: Array[Option[T]],
                               valueArray: Array[Option[T]],
                               baseClass: String,
                               colType: String,
                               rowIndex: Int,
                               colIndex: Int,
                               related: Int,
                               nsAlias: String,
                               nsFull: String,
                               attr: String,
                               attrType: String,
                               card: Int,
                               enums: Seq[String]
                              )(implicit ctx: Ctx.Owner)
  extends BodyElements with Update {

  val attrFull   = s":$nsFull/${clean(attr)}"
  val enumPrefix = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"

  // Tx update coordinates
  val (
    eIndex,
    tArray, tIndex,
    txArray, txIndex,
    txInstantArray, txInstantIndex
    ) = cols.foldLeft(
    0,
    Option.empty[Array[Option[Double]]], 0,
    Option.empty[Array[Option[Double]]], 0,
    Option.empty[Array[Option[String]]], 0
  ) {
    case (_, Col(colIndex1, _, `nsAlias`, `nsFull`, "e", _, _, _, _, _, _, _, _, _, _)) =>
      (colIndex1, None, 0, None, 0, None, 0)

    case (
      (eColIndex, _, _, _, _, _, _),
      Col(colIndex1, _, _, `nsFull`, `attr`, _, _, _, _, _, _, "t", _, _, _)
      ) =>
      (eColIndex,
        Some(qr.num(qr.arrayIndexes(colIndex1))), colIndex1, None, 0, None, 0)

    case (
      (eColIndex, tArray, tIndex, _, _, _, _),
      Col(colIndex1, _, _, `nsFull`, `attr`, _, _, _, _, _, _, "tx", _, _, _)
      ) =>
      (eColIndex, tArray, tIndex,
        Some(qr.num(qr.arrayIndexes(colIndex1))), colIndex1, None, 0)

    case (
      (eColIndex, tArray, tIndex, txArray, txIndex, _, _),
      Col(colIndex1, _, _, `nsFull`, `attr`, _, _, _, _, _, _, "txInstant", _, _, _)
      ) =>
      (eColIndex, tArray, tIndex, txArray, txIndex,
        Some(qr.str(qr.arrayIndexes(colIndex1))), colIndex1)

    case (acc, _) => acc
  }


  def update(cellId: String,
             cell: TableCell,
             row: TableRow,
             eid: Long,
             oldVOpt: Option[T],
             isNum: Boolean): Unit

  def updateClient(t: Long, tx: Long, txInstant: String,
                   cellId: String,
                   cell: TableCell,
                   row: TableRow,
                   eid: Long,
                   oldVOpt: Option[T],
                   isNum: Boolean,
                   newValueOpt: Option[T]
                  ): Unit = {

    // Update current tx data
    curT() = t
    curTx() = tx
    curTxInstant() = txInstant

    // Force marking the entity changed
    curEntity() = eid

    // don't know why we need to poke curEntity twice to redraw Entity view...
    curEntity.recalc()
    entityHistorySort.recalc()

    // Update cell(s) of row

    val (tString, txString) = (t.toString, tx.toString)

    val tLambda = if (tIndex > 0) {
      (rowCells: NodeList) => {
        val tCell = rowCells.item(tIndex + 1).asInstanceOf[TableCell]
        tCell.textContent = tString
        tCell.className = "txChosen"
      }
    } else (_: NodeList) => ()

    val txLambda = if (tIndex > 0) {
      (rowCells: NodeList) => {
        val txCell = rowCells.item(txIndex + 1).asInstanceOf[TableCell]
        txCell.textContent = txString
        txCell.className = "txChosen"
      }
    } else (_: NodeList) => ()

    val txInstantLambda = if (tIndex > 0) {
      (rowCells: NodeList) => {
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

    var i = 0
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

    // Lambda to update arrays in client memory
    val updateArrays = {
      val (tDouble, txDouble) = (t.toDouble, tx.toDouble)
      (tArray, txArray, txInstantArray) match {
        case (None, None, None)                                     =>
          (rowIndex: Int) => {
            valueArray(rowIndex) = newValueOpt
          }
        case (Some(tArray1), None, None)                            =>
          (rowIndex: Int) => {
            valueArray(rowIndex) = newValueOpt
            tArray1(rowIndex) = Some(tDouble)
          }
        case (None, Some(txArray1), None)                           =>
          (rowIndex: Int) => {
            valueArray(rowIndex) = newValueOpt
            txArray1(rowIndex) = Some(txDouble)
          }
        case (None, None, Some(txInstantArray1))                    =>
          (rowIndex: Int) => {
            valueArray(rowIndex) = newValueOpt
            txInstantArray1(rowIndex) = Some(txInstant)
          }
        case (Some(tArray1), None, Some(txInstantArray1))           =>
          (rowIndex: Int) => {
            valueArray(rowIndex) = newValueOpt
            tArray1(rowIndex) = Some(tDouble)
            txInstantArray1(rowIndex) = Some(txInstant)
          }
        case (None, Some(txArray1), Some(txInstantArray1))          =>
          (rowIndex: Int) => {
            valueArray(rowIndex) = newValueOpt
            txArray1(rowIndex) = Some(txDouble)
            txInstantArray1(rowIndex) = Some(txInstant)
          }
        case (Some(tArray1), Some(txArray1), None)                  =>
          (rowIndex: Int) => {
            valueArray(rowIndex) = newValueOpt
            tArray1(rowIndex) = Some(tDouble)
            txArray1(rowIndex) = Some(txDouble)
          }
        case (Some(tArray1), Some(txArray1), Some(txInstantArray1)) =>
          (rowIndex: Int) => {
            valueArray(rowIndex) = newValueOpt
            tArray1(rowIndex) = Some(tDouble)
            txArray1(rowIndex) = Some(txDouble)
            txInstantArray1(rowIndex) = Some(txInstant)
          }
      }
    }

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
