package moleculeadmin.client.app.domain.query.grouped

import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.TypeValidation
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
  with GroupedAttrElements with KeyEvents with FilterFactory with TypeValidation {

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

  // Selected triples of rowIndex/value/count
  var selected = Seq.empty[(Int, String, Int)]

  // Current ordering of grouped data
  var curOrdering = 1

  val spanOfSelected   = span().render
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
        rowMaker(rowIndex, groupedData(rowIndex))
      )
      rowIndex += 1
    }
  }

  def showGrouped(): Unit = {
    spanOfSelected.appendChild(_groupedSelected(colType, selected, toggleOff))
    spanOfSelected.appendChild(_separator)
  }

  def setFilter(): Unit = {
    val filterExpr = if (attrType == "String") {
      if (selected.length == 1 && selected.head._2 == "-")
        "-"
      else
        "/" + selected.map(_._2).mkString("\n/")
    } else {
      selected.map(_._2).mkString("\n")
    }

    val filter = createFilter(col, filterExpr, splitComma = false).get
    filters() = filters.now.filterNot(_._1 == colIndex) + (colIndex -> filter)
  }

  def toggleOn(rowIndex: Int, curV: String, count: Int): Unit = {
    document.getElementById(s"grouped-row-$rowIndex")
      .setAttribute("class", "selected")
    spanOfSelected.innerHTML = ""
    if (selected.map(_._2).contains("-")) {
      selected = selected.flatMap {
        case (rowIndex1, "-", _) =>
          // Un-mark grouped value for None value
          document.getElementById(s"grouped-row-$rowIndex1")
            .removeAttribute("class")
          None
        case s                   => Some(s)
      } :+ (rowIndex, curV, count)
    } else if (curV == "-" && selected.nonEmpty) {
      // Un-mark previous grouped when applying None
      selected.foreach {
        case (rowIndex1, _, _) =>
          document.getElementById(s"grouped-row-$rowIndex1")
            .removeAttribute("class")
      }
      // Mark None only
      selected = Seq((rowIndex, curV, count))
    } else {
      selected = selected :+ (rowIndex, curV, count)
    }
    showGrouped()
    setFilter()
  }

  def toggleOff(rowIndex: Int, curV: String, count: Int): Unit = {
    document.getElementById(s"grouped-row-$rowIndex")
      .removeAttribute("class")
    spanOfSelected.innerHTML = ""
    selected = selected.filterNot(_ == (rowIndex, curV, count))
    if (selected.isEmpty) {
      filters() = filters.now - colIndex
    } else {
      showGrouped()
      setFilter()
    }
  }

  val toggle = (rowIndex: Int, curV: String, count: Int) => () =>
    if (selected.contains((rowIndex, curV, count)))
      toggleOff(rowIndex, curV, count)
    else
      toggleOn(rowIndex, curV, count)


  val update = updateLambda(tableRows, valueColIndex, eidArray, valueArray)

  def rowId(rowIndex: Int) = s"grouped-row-$rowIndex"

  val rowMaker: (Int, (String, Int)) => TableRow = {
    if (colType == "double") {
      (rowIndex: Int, vc: (String, Int)) =>
        _rowNum(rowId(rowIndex), rowIndex, vc._1, vc._2,
          update(rowIndex), toggle(rowIndex, vc._1, vc._2))
    } else {
      attrType match {
        case "String" =>
          (rowIndex: Int, vc: (String, Int)) =>
            _rowStr(rowId(rowIndex), rowIndex, vc._1, vc._2,
              update(rowIndex), toggle(rowIndex, vc._1, vc._2))

        case "Date" =>
          (rowIndex: Int, vc: (String, Int)) =>
            _rowStr(rowId(rowIndex), rowIndex, vc._1, vc._2,
              update(rowIndex), toggle(rowIndex, vc._1, vc._2))
      }
    }
  }
}
