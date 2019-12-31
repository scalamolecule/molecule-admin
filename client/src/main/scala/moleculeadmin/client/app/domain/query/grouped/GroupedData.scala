package moleculeadmin.client.app.domain.query.grouped

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.edit.{Update, UpdateClient}
import moleculeadmin.client.app.element.query.GroupedAttrElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.data.FilterFactory
import org.scalajs.dom.html.{Span, TableCell, TableRow, TableSection}
import org.scalajs.dom.raw.{HTMLInputElement, NodeList}
import org.scalajs.dom.{document, window}
import rx.Ctx
import scalatags.JsDom.all._
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global


case class GroupedData[T](
  qr: QueryResult,
  c: Col,
  grouped: Span,
)(implicit ctx: Ctx.Owner)
  extends GroupedAttrElements with KeyEvents with FilterFactory with Update {

  type keepBooPickleImport_GroupedData = PickleState

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType, _,
  opt, enums, _, _, _, _, _) = c

  val attrFull      = s":$nsFull/${clean(attr)}"
  val enumPrefix    = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"
  val isNum         = Seq("Int", "Long", "Float", "Double").contains(attrType)
  val tableRows     = document.getElementById("tableBody").childNodes
  val valueColIndex = colIndex + 1
  // Tx update coordinates
  val (
    eIndex,
    tArray, tIndex,
    txArray, txIndex,
    txInstantArray, txInstantIndex
    )               = getTxArrays(columns.now, qr, nsAlias, nsFull, attr)
  //  val eidIndex      = getEidColIndex(columns.now, colIndex, nsAlias, nsFull)
  val eidArray      = qr.num(eIndex)
  val arrayIndex    = qr.arrayIndexes(c.colIndex)
  val valueArray    =
    (if (colType == "double")
      qr.num(arrayIndex)
    else
      qr.str(arrayIndex)).asInstanceOf[Array[Option[T]]]

  // Local cache of selected triples of rowNumber/value/count
  var selected = Seq.empty[(Int, String, Int)]

  def showGrouped(): Unit = {
    grouped.appendChild(_groupedSelected(colType, selected, toggleOff))
    grouped.appendChild(_separator)
  }

  def setFilter(): Unit = {
    val filterExpr = if (attrType == "String")
      "/" + selected.map(_._2).mkString("\n/")
    else
      "" + selected.map(_._2).mkString("\n")

    val filter = createFilter(c, filterExpr, splitComma = false).get
    filters() = filters.now.filterNot(_._1 == colIndex) + (colIndex -> filter)
  }

  def toggleOn(n: Int, curV: String, count: Int): Unit = {
    document.getElementById(s"grouped-row-$colIndex-$n")
      .setAttribute("class", "selected")
    grouped.innerHTML = ""
    selected = selected :+ (n, curV, count)
    showGrouped()
    setFilter()
  }

  def toggleOff(n: Int, curV: String, count: Int): Unit = {
    document.getElementById(s"grouped-row-$colIndex-$n").removeAttribute("class")
    grouped.innerHTML = ""
    selected = selected.filterNot(_ == (n, curV, count))
    if (selected.isEmpty) {
      filters() = filters.now - colIndex
    } else {
      showGrouped()
      setFilter()
    }
  }

  val update = (n: Int, oldStr: String, count: Int) => () => {
    val groupedCell    = document.getElementById(s"grouped-cell-$colIndex-$n")
      .asInstanceOf[HTMLInputElement]
    val newStr: String = _html2str(groupedCell.innerHTML)

    // Optional value for in-memory Client value array
    val newVopt: Option[T] = {
      if (newStr.isEmpty) {
        Option.empty[T]
      } else if (isNum) {
        Try(newStr.toDouble) match {
          case Success(n) => Some(n).asInstanceOf[Option[T]]
          case Failure(_) =>
            // Catch invalid str in validation below
            None
        }
      } else {
        Some(newStr).asInstanceOf[Option[T]]
      }
    }
    val oldVopt: Option[T] =
      if (oldStr.isEmpty)
        None
      else if (colType == "double")
        Some(oldStr.toDouble.asInstanceOf[T])
      else
        Some(oldStr.asInstanceOf[T])

    if (editCellId.nonEmpty
      && editCellId == groupedCell.id
      && oldStr != newStr
    ) {
      if (attrType != "String" && newStr.contains('\n')) {
        editCellId = ""
        window.alert(
          s"Can't save multiple $attrFull values of type `$attrType`:\n$newStr"
        )
        groupedCell.focus()

      } else if (enums.nonEmpty && !enums.contains(newStr)) {
        editCellId = ""
        window.alert(
          s"Can't save non-defined $attrFull enum value `$newStr`" +
            "\nThe following enum values are defined:\n  " +
            enums.mkString("\n  ")
        )
        groupedCell.focus()

      } else if (!valid(attrType, newStr)) {
        editCellId = ""
        window.alert(s"Invalid $attrFull value of type `$attrType`:\n$newStr")
        groupedCell.focus()

      } else {
        val nonEmpty = groupedCell.textContent.trim.nonEmpty
        val value    = if (nonEmpty) newStr else oldStr

        // Update edit cell in grouped data
        groupedCell.innerHTML = ""
        if (nonEmpty) {
          if (attrType == "String") {
            groupedCell.appendChild(_str2frags(value).render)
          } else {
            groupedCell.appendChild(value.render)
          }
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

        var affectedRows = List.empty[Int]
        var i            = 0
        while (i < tableRows.length) {
          val cell  = tableRows.item(i).childNodes.item(valueColIndex).asInstanceOf[TableCell]
          val cellV = html2value(cell.innerHTML)
          if (oldStr == cellV) {
            affectedRows = affectedRows :+ i
            cell.innerHTML = ""
            cell.appendChild(value2node(newStr))
          }
          i += 1
        }

        // Update value array and collect entity ids
        val filterIndex = queryCache.filterIndex
        val indexBridge = {
          if (filterIndex.nonEmpty)
            (i: Int) => filterIndex(i)
          else
            (i: Int) => i
        }
        val length    = actualRowCount // (filtered)
        val positives = new Array[Int](length)
        var eids      = List.empty[Long]
        i = 0
        var posIndex = 0
        var j = 0
        while (i < length) {
          j = indexBridge(i)
          if (valueArray(j) == oldVopt) {
            eids = eidArray(j).get.toLong :: eids
            positives(posIndex) = j
            valueArray(j) = newVopt
            posIndex += 1
          }
          i += 1
        }

        val affectedIndexes = new Array[Int](posIndex)
        System.arraycopy(positives, 0, affectedIndexes, 0, posIndex)

        // Update db
        val save = if (colType == "double") {
          val v    = value.toDouble
          val data = if (nonEmpty)
            eids.map((_, Nil, Seq(v)))
          else
            eids.map((_, Seq(v), Nil))
          queryWire().updateNum(db, attrFull, attrType, data).call()
        } else {
          val data = if (nonEmpty)
            eids.map((_, Nil, Seq(value)))
          else
            eids.map((_, Seq(value), Nil))
          queryWire().updateStr(db, attrFull, attrType, enumPrefix, data).call()
        }
        save.foreach {
          case Right((t, tx, txInstant)) =>
            // Update client tx cells and arrays
            UpdateClientTx(
              qr, valueArray, nsAlias, nsFull, attr, attrType, enums
            ).updateClient(
              t, tx, txInstant,
              tableRows, newVopt, oldVopt, valueColIndex,
              affectedRows, affectedIndexes
            )

            if (nonEmpty)
              println(s"Updated ${eids.length} entities: $attrFull value from " +
                s"`$oldStr` to `$newStr`")
            else
              println(s"Successfully retracted $attrFull value `$oldStr` " +
                s"of ${eids.length} entities")

          case Left(err) =>
            editCellId = ""
            selectContent(groupedCell)
            if (nonEmpty)
              window.alert(s"Error updating $attrFull value from " +
                s"`$oldStr` to `$newStr`:\n$err")
            else
              window.alert(s"Error retracting $attrFull value `$oldStr` " +
                s"of ${eids.length} entities:\n$err")
            groupedCell.focus()
        }
      }
    }
  }

  val toggle = (n: Int, curV: String, count: Int) => () =>
    if (selected.contains((n, curV, count)))
      toggleOff(n, curV, count)
    else
      toggleOn(n, curV, count)


  def appendRows(
    tableBody: TableSection,
    groupedData: Seq[(String, Int)],
    first: Int,
    last: Int
  ): Unit = {
    val rowMaker = _rowMaker(colIndex, colType, attrType, update, toggle)
    var rowIndex = first
    while (rowIndex < last) {
      tableBody.appendChild(
        rowMaker(rowIndex + 1, groupedData(rowIndex))
      )
      rowIndex += 1
    }
  }

  def getData: Seq[(String, Int)] = {
    val filterIndex = queryCache.filterIndex
    val indexBridge = {
      if (filterIndex.nonEmpty)
        (i: Int) => filterIndex(i)
      else
        (i: Int) => i
    }

    colType match {
      case "string" if !opt =>
        val valueArray = qr.str(qr.arrayIndexes(colIndex))
        var rowIndex   = 0
        val lastRow    = actualRowCount
        val vs1        = new Array[String](lastRow)
        while (rowIndex < lastRow) {
          vs1(rowIndex) = valueArray(indexBridge(rowIndex)).get match {
            case v if v.trim.isEmpty => s"{$v}"
            case v                   => v
          }
          rowIndex += 1
        }
        vs1.groupBy(identity).mapValues(_.length).toSeq
          .sortBy { case (v, c) => (-c, v) }

      case "double" if !opt =>
        val valueArray = qr.num(qr.arrayIndexes(colIndex))
        var rowIndex   = 0
        val lastRow    = actualRowCount
        val vs1        = new Array[Double](lastRow)
        while (rowIndex < lastRow) {
          vs1(rowIndex) = valueArray(indexBridge(rowIndex)).get
          rowIndex += 1
        }
        vs1.groupBy(identity).mapValues(_.length).toSeq
          .sortBy { case (v, c) => (-c, v) }
          .map { case (v, c) => (v.toString, c) }


      case "string" =>
        val (nil, vs) = qr.str(qr.arrayIndexes(colIndex)).toList.foldLeft(
          List.empty[Int], List.empty[String]
        ) {
          case ((nil, vs), None) => (nil :+ 1, vs)
          // todo: format empty string/line shifts
          case ((nil, vs), Some(v)) if v.trim.isEmpty => (nil, vs :+ s"{$v}")
          case ((nil, vs), Some(v))                   => (nil, vs :+ v)
        }
        ("<nil>", nil.length) +: vs.groupBy(identity)
          .mapValues(_.length).toSeq
          .sortBy { case (v, c) => (-c, v) }

      case "double" =>
        val (nil, vs) = qr.num(qr.arrayIndexes(colIndex)).toList.foldLeft(
          List.empty[Int], List.empty[Double]
        ) {
          case ((nil, vs), None)    => (nil :+ 1, vs)
          case ((nil, vs), Some(v)) => (nil, vs :+ v)
        }
        ("<nil>", nil.length) +: vs.groupBy(identity)
          .mapValues(_.length).toSeq
          .sortBy { case (v, c) => (-c, v) }
          .map { case (v, c) => (v.toString, c) }

      case _ => Nil
    }
  }
}
