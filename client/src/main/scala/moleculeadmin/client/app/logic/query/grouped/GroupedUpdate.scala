package moleculeadmin.client.app.logic.query.grouped

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.TypeValidation
import moleculeadmin.client.app.html.query.GroupedAttrElements
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.Col
import org.scalajs.dom.html.TableCell
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{NodeList, document, window}
import rx.Ctx
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


abstract class GroupedUpdate[T](col: Col)(implicit ctx: Ctx.Owner)
  extends GroupedData[T](col) with GroupedAttrElements with TypeValidation {

  /*
    3 data representations for each old/new pair:
    - in groupedDate, internal array cache and database:
      None/Some(T) where T is String/Double
    - in grouped view: "__none__" / "string"
    - in data table cells: empty cell / cell with "string"
   */

  def updateLambda(tableRows: NodeList): Int => () => Unit = {
    rowIndex: Int =>
      () => {
        val (oldTopt, count) = groupedData(rowIndex)
        val groupedCell      = document.getElementById(cellId(rowIndex))
          .asInstanceOf[HTMLInputElement]
        val newGroupedStrRaw = groupedCell.innerHTML.trim

        val (isValue, newGroupedStr, newTableStr) = newGroupedStrRaw match {
          case "" | `none` => (false, none, "")
          case s           => (true, _html2str(s), _html2str(s))
        }

        val newTopt                      = getNewTopt(!isValue, newGroupedStr)
        val (oldGroupedStr, oldTableStr) = oldTopt match {
          case None    => (none, "")
          case Some(v) => (v.toString, v.toString)
        }

        if (editCellId.nonEmpty
          && editCellId == groupedCell.id
          && oldGroupedStr != newGroupedStr
        ) {
          if (attrType != "String" && newGroupedStr.contains('\n')) {
            editCellId = ""
            window.alert(
              s"Can't save multiple $attrFull values of type `$attrType`:\n$newGroupedStr"
            )
            groupedCell.focus()

          } else if (enums.nonEmpty && !enums.contains(newGroupedStr)) {
            editCellId = ""
            window.alert(
              s"Can't save non-defined $attrFull enum value `$newGroupedStr`" +
                "\nThe following enum values are defined:\n  " +
                enums.mkString("\n  ")
            )
            groupedCell.focus()

          } else if (isValue && !valid(attrType, newGroupedStr)) {
            editCellId = ""
            window.alert(s"Invalid $attrFull value of type `$attrType`:\n$newGroupedStr")
            groupedCell.focus()

          } else {
            updating(
              tableRows, groupedCell, count, rowIndex,
              oldGroupedStr,
              newGroupedStr,
              oldTopt,
              newTopt,
              isValue,
              oldTableStr,
              newTableStr,
            )
          }
        }
      }
  }

  def getNewTopt(isNone: Boolean, newGroupedStr: String): Option[T] = {
    if (isNone) {
      Option.empty[T]
    } else if (isNum) {
      Try(newGroupedStr.toDouble) match {
        case Success(n) => Some(n).asInstanceOf[Option[T]]
        case Failure(_) =>
          // Catch invalid str in validation below
          None
      }
    } else {
      Some(newGroupedStr).asInstanceOf[Option[T]]
    }
  }

  def updating(
    tableRows: NodeList,
    groupedCell: HTMLInputElement,
    count: Int,
    rowIndex: Int,
    oldGroupedStr: String,
    newGroupedStr: String,
    oldTopt: Option[T],
    newTopt: Option[T],
    nonEmpty: Boolean,
    oldTableStr: String,
    newTableStr: String,
  ): Unit = {
    // Update grouped data
    groupedData = groupedData.updated(rowIndex, (newTopt, count))

    // Update edit cell in grouped data table
    groupedCell.innerHTML = ""
    if (nonEmpty) {
      if (attrType == "String") {
        groupedCell.appendChild(_str2frags(newGroupedStr).render)
      } else {
        groupedCell.appendChild(newGroupedStr.render)
      }
    } else {
      groupedCell.appendChild(none.render)
    }

    // Update values in data table
    val (html2value, value2node) = attrType match {
      case "String" => (
        (html: String) => _html2str(html),
        (v: String) => _str2frags(v).render
      )
      case _        => (
        (html: String) => html,
        (v: String) => v.render
      )
    }

    var affectedRows    = List.empty[Int]
    var i               = 0
    val tableRowsLength = tableRows.length
    while (i < tableRowsLength) {
      val tableCell  = tableRows.item(i).childNodes.item(valueColIndex).asInstanceOf[TableCell]
      val tableCellV = html2value(tableCell.innerHTML)
      if (tableCellV == oldTableStr) {
        affectedRows = affectedRows :+ i
        tableCell.innerHTML = ""
        if (nonEmpty)
          tableCell.appendChild(value2node(newTableStr))
      }
      i += 1
    }

    // Update value array and collect entity ids
    val filterIndex      = queryCache.filterIndex
    val indexBridge      = {
      if (filterIndex.nonEmpty)
        (i: Int) => filterIndex(i)
      else
        (i: Int) => i
    }
    val filteredRowCount = actualRowCount
    val positives        = new Array[Int](filteredRowCount)
    var eids             = List.empty[Long]
    i = 0
    var posIndex = 0
    var j        = 0
    while (i < filteredRowCount) {
      j = indexBridge(i)
      if (valueArray(j) == oldTopt) {
        eids = eidArray(j).get.toLong :: eids
        positives(posIndex) = j
        valueArray(j) = newTopt
        posIndex += 1
      }
      i += 1
    }

    val affectedIndexes = new Array[Int](posIndex)
    System.arraycopy(positives, 0, affectedIndexes, 0, posIndex)

    // Update db
    val saveLambdas = if (colType == "double") {
      val data = if (nonEmpty)
        eids.map((_, Nil, Seq(newGroupedStr.toDouble)))
      else
        eids.map((_, Seq(oldGroupedStr.toDouble), Nil))
      queryWireAjax().updateNum(db, attrFull, attrType, data).call()

    } else {

      val data = if (nonEmpty)
        eids.map((_, Nil, Seq(newGroupedStr)))
      else
        eids.map((_, Seq(oldGroupedStr), Nil))
      queryWireAjax().updateStr(db, attrFull, attrType, enumPrefix, data).call()
    }

    saveLambdas.foreach {
      case Right((t, tx, txInstant)) =>
        // Update client tx cells and arrays
        GroupedUpdateClient(
          qr, valueArray, nsAlias, nsFull, attr, attrType, enums
        ).updateClient(
          t, tx, txInstant,
          tableRows, newTopt, valueColIndex,
          affectedRows, affectedIndexes
        )

        if (nonEmpty)
          println(s"Updated ${eids.length} entities: $attrFull value from " +
            s"`$oldGroupedStr` to `$newGroupedStr`")
        else
          println(s"Successfully retracted $attrFull value `$oldGroupedStr` " +
            s"of ${eids.length} entities")

      case Left(err) =>
        editCellId = ""
        selectContent(groupedCell)
        if (nonEmpty)
          window.alert(s"Error updating $attrFull value from " +
            s"`$oldGroupedStr` to `$newGroupedStr`:\n$err")
        else
          window.alert(s"Error retracting $attrFull value `$oldGroupedStr` " +
            s"of ${eids.length} entities:\n$err")
        groupedCell.focus()
    }
  }

}
