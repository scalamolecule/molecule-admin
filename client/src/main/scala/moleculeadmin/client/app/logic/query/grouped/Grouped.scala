package moleculeadmin.client.app.logic.query.grouped

import moleculeadmin.client.app.logic.query.KeyEvents
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.TypeValidation
import moleculeadmin.client.app.html.query.GroupedAttrElements
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

  // Local caches

  // Selected triples of rowIndex/value/count
  var selected = Seq.empty[(Int, Option[T], Int)]

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
    val previewCount = 20

    if (count > previewCount) {
      // Render first 10 immediately
      appendRows(groupedTableBody, 0, previewCount)
      // Render the rest afterwards in the background
      setTimeout(200) {
        appendRows(groupedTableBody, previewCount, count)
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
    val filterExpr = selected.map {
      case (_, None, _)    => "-"
      case (_, Some(v), _) => v
    }.mkString("\n")
    val filter = createFilter(col, filterExpr, splitComma = false).get
    filters() = filters.now.filterNot(_._1 == colIndex) + (colIndex -> filter)
  }

  def toggleOn(rowIndex: Int, curVopt: Option[T], count: Int): Unit = {
    document.getElementById(rowId(rowIndex)).setAttribute("class", "selected")
    spanOfSelected.innerHTML = ""
    selected = selected :+ (rowIndex, curVopt, count)
    showGrouped()
    setFilter()
  }

  def toggleOff(rowIndex: Int, curVopt: Option[T], count: Int): Unit = {
    document.getElementById(rowId(rowIndex)).removeAttribute("class")
    spanOfSelected.innerHTML = ""
    selected = selected.filterNot(_ == (rowIndex, curVopt, count))
    if (selected.isEmpty) {
      filters() = filters.now - colIndex
    } else {
      showGrouped()
      setFilter()
    }
  }

  val toggle = (rowIndex: Int, curVopt: Option[T], count: Int) => () =>
    if (selected.contains((rowIndex, curVopt, count)))
      toggleOff(rowIndex, curVopt, count)
    else
      toggleOn(rowIndex, curVopt, count)


  val update = updateLambda(document.getElementById("tableBody").childNodes)

  val rowMaker: (Int, (Option[T], Int)) => TableRow = {
    if (colType == "double") {
      (rowIndex: Int, vc: (Option[T], Int)) =>
        _rowNum(
          rowId(rowIndex),
          cellId(rowIndex),
          rowIndex,
          vc._1.fold("__none__")(_.toString),
          vc._2,
          update(rowIndex),
          toggle(rowIndex, vc._1, vc._2)
        )
    } else {
      // todo special case for Date and other types?
      (rowIndex: Int, vc: (Option[T], Int)) =>
        _rowStr(
          rowId(rowIndex),
          cellId(rowIndex),
          rowIndex,
          vc._1.fold("__none__")(_.toString),
          vc._2,
          update(rowIndex),
          toggle(rowIndex, vc._1, vc._2)
        )
    }
  }
}
