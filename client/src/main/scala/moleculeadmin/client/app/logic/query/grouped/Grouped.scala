package moleculeadmin.client.app.logic.query.grouped

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.data.FilterFactory
import org.scalajs.dom.document
import org.scalajs.dom.html.{Element, TableRow, TableSection}
import rx.Ctx
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.scalajs.js.timers.setTimeout


case class Grouped[T](col: Col)(implicit ctx: Ctx.Owner)
  extends GroupedUpdate[T](col) with FilterFactory {

  // Local caches

  // Selected triples of rowIndex/value/count
  //  var selected = Seq.empty[(Int, Option[T], Int)]
  private var selected = Seq.empty[(Int, Option[String], Int)]

  // Current ordering of grouped data
  private var curOrdering = 1

  private val spanOfSelected   = span().render
  private val groupedTableBody = _groupedTableBody(colIndex)


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
    val countAll = groupedData.length
    // Avoid out-of-memory rendering more than 25000 rows in group table
    // Of course depends of data size and hardware
    val count    = countAll.min(25000)
    val headRow  = _headRow(colType, count, sort, ordering)

    groupedTableBody.innerHTML = ""
    groupedTableBody.appendChild(headRow)
    val previewCount = 20

    if (count > previewCount) {
      // Render first 20 (visible) immediately
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

  private val rowMaker: (Int, (Option[String], Int)) => TableRow = {
    colType match {
      case "double" | "listDouble" =>
        (rowIndex: Int, vc: (Option[String], Int)) =>
          _rowNum(
            rowId(rowIndex),
            cellId(rowIndex),
            rowIndex,
            vc._1.getOrElse(none),
            vc._2,
            update(rowIndex),
            toggle(rowIndex, vc._1, vc._2)
          )

      case _ =>
        (rowIndex: Int, vc: (Option[String], Int)) =>
          _rowStr(
            rowId(rowIndex),
            cellId(rowIndex),
            rowIndex,
            vc._1,
            vc._2,
            update(rowIndex),
            toggle(rowIndex, vc._1, vc._2)
          )
    }
  }

  private val update = updateLambda(
    document.getElementById("tableBody").childNodes)

  private val toggle = {
    (rowIndex: Int, curVopt: Option[String], count: Int) =>
      () =>
        if (selected.contains((rowIndex, curVopt, count)))
          toggleOff(rowIndex, curVopt, count)
        else
          toggleOn(rowIndex, curVopt, count)
  }

  def showGrouped(): Unit = {
    spanOfSelected.appendChild(_groupedSelected(colType, selected, toggleOff))
    spanOfSelected.appendChild(_separator)
  }

  def toggleOn(rowIndex: Int, curVopt: Option[String], count: Int): Unit = {
    document.getElementById(rowId(rowIndex)).setAttribute("class", "selected")
    spanOfSelected.innerHTML = ""
    selected = selected :+ (rowIndex, curVopt, count)
    showGrouped()
    setFilter()
  }

  def toggleOff(rowIndex: Int, curVopt: Option[String], count: Int): Unit = {
    document.getElementById(rowId(rowIndex)).removeAttribute("class")
    spanOfSelected.innerHTML = ""
    selected = selected.filterNot(_ == (rowIndex, curVopt, count))
    if (selected.isEmpty) {
      removeFilter()
    } else {
      showGrouped()
      setFilter()
    }
  }

  def setFilter(): Unit = {
    val filterExpr = selected.map {
      case (_, None, _)    => "-"
      case (_, Some(v), _) => v
    }.mkString("\n")
    val filter     = createFilter(col, filterExpr, splitComma = false).get
    // Let only columns trigger
    filters.kill()
    filters() = filters.now.filterNot(_._1 == colIndex) + (colIndex -> filter)
    columns.recalc()
  }

  def removeFilter(): Unit = {
    // Let only columns trigger
    filters.kill()
    filters() = filters.now - colIndex
    columns.recalc()
  }
}
