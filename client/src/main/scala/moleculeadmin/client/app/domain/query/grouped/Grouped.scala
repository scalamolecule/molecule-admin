package moleculeadmin.client.app.domain.query.grouped

import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.edit.Update
import moleculeadmin.client.app.element.query.GroupedAttrElements
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.data.FilterFactory
import org.scalajs.dom.document
import org.scalajs.dom.html.{Element, TableRow, TableSection}
import rx.Ctx
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.scalajs.js.timers.setTimeout


case class Grouped[T](col: Col)
  (implicit ctx: Ctx.Owner) extends GroupedUpdate[T](col)
  with GroupedAttrElements with KeyEvents with FilterFactory with Update {

  val tableRows     = document.getElementById("tableBody").childNodes
  val valueColIndex = colIndex + 1
  val eidIndex      = getEidColIndex(columns.now, colIndex, nsAlias, nsFull)
  val eidArray      = qr.num(eidIndex)
  val arrayIndex    = qr.arrayIndexes(col.colIndex)
  val valueArray    =
    (if (colType == "double")
      qr.num(arrayIndex)
    else
      qr.str(arrayIndex)).asInstanceOf[Array[Option[T]]]


  // Local caches

  // Selected triples of groupRowNumber/value/count
  var selected = Seq.empty[(Int, String, Int)]

  // Current ordering of grouped data
  var curOrdering = 1

  val spanOfSelected = span().render
  val groupedTableBody = _groupedTableBody(colIndex)

  def render: JsDom.TypedTag[Element] = {
    extractGroupedData()
    populate(1)
    _groupedCard(
      s"$nsFull/$attr",
      spanOfSelected,
      _groupedTable(groupedTableBody)
    )
  }

  val sort = (sortType: String) => {
    sortType match {
      case "count" => () => {
        curOrdering match {
          case 1 => populate(2)
          case _ => populate(1)
        }
      }
      case "value" => () => {
        curOrdering match {
          case 3 => populate(4)
          case _ => populate(3)
        }
      }
    }
  }

  def populate(ordering: Int): Unit = {
    curOrdering = ordering
    sortData(ordering)
    val count   = groupedData.length
    val headRow = _headRow(colType, count, sort, ordering)

    groupedTableBody.innerHTML = ""
    groupedTableBody.appendChild(headRow)

    if (count > 10) {
      // Render first 10 immediately
      appendRows(groupedTableBody, 0, 10)
      // Render the rest afterwards in the background
      setTimeout(200) {
        appendRows(groupedTableBody, 10, count)
      }
    } else {
      appendRows(groupedTableBody, 0, count)
    }
  }


  def appendRows(
    groupedTableBody: TableSection,
    first: Int,
    last: Int
  ): Unit = {
    var rowIndex = first
    while (rowIndex < last) {
      groupedTableBody.appendChild(
        rowMaker(rowIndex + 1, groupedData(rowIndex))
      )
      rowIndex += 1
    }
  }

  def showGrouped(): Unit = {
    spanOfSelected.appendChild(_groupedSelected(colType, selected, toggleOff))
    spanOfSelected.appendChild(_separator)
  }

  def setFilter(): Unit = {
    val filterExpr = if (attrType == "String")
      "/" + selected.map(_._2).mkString("\n/")
    else
      selected.map(_._2).mkString("\n")

    val filter = createFilter(col, filterExpr, splitComma = false).get
    filters() = filters.now.filterNot(_._1 == colIndex) + (colIndex -> filter)
  }

  def toggleOn(n: Int, curV: String, count: Int): Unit = {
    document.getElementById(s"grouped-row-$colIndex-$n")
      .setAttribute("class", "selected")
    spanOfSelected.innerHTML = ""
    selected = selected :+ (n, curV, count)
    showGrouped()
    setFilter()
  }

  def toggleOff(n: Int, curV: String, count: Int): Unit = {
    document.getElementById(s"grouped-row-$colIndex-$n")
      .removeAttribute("class")
    spanOfSelected.innerHTML = ""
    selected = selected.filterNot(_ == (n, curV, count))
    if (selected.isEmpty) {
      filters() = filters.now - colIndex
    } else {
      showGrouped()
      setFilter()
    }
  }

  val toggle = (n: Int, curV: String, count: Int) => () =>
    if (selected.contains((n, curV, count)))
      toggleOff(n, curV, count)
    else
      toggleOn(n, curV, count)


  val update = updateLambda(tableRows, valueColIndex, eidArray, valueArray)

  val rowMaker: (Int, (String, Int)) => TableRow = {
    if (colType == "double") {
      (i: Int, row: (String, Int)) =>
        _rowNum(colIndex, i, row._1, row._2, update, toggle)
    } else {
      attrType match {
        case "String" =>
          (i: Int, row: (String, Int)) =>
            _rowStr(colIndex, i, row._1, row._2, update, toggle)

        case "Date" =>
          (i: Int, row: (String, Int)) =>
            _rowStr(colIndex, i, row._1, row._2, update, toggle)
      }
    }
  }
}
