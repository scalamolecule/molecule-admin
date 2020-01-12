package moleculeadmin.client.app.element.query

import moleculeadmin.client.app.element.query.datatable.HeadElements
import org.scalajs.dom.html._
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


trait UndoElements extends SubMenuElements with HeadElements {

  def _groupedCard(
    header: String,
    spanOfSelected: Span,
    values: Table
  ): TypedTag[Element] =
    _card(
      _cardHeader(h5(header)),
      _cardBody(
        padding := 0,
        spanOfSelected,
        values
      )
    )

  def _groupedSelected(
    colType: String,
    vs: Seq[(Int, String, Int)],
    toggleOff: (Int, String, Int) => Unit
  ): Table = {
    table(
      cls := "tableGroupedSelected",
      vs.map { case (n, v, c) =>
        tr(
          onclick := { () => toggleOff(n, v, c) },
          td(
            width := "5%",
            padding := "0px 7px",
            span(
              cls := "oi oi-x",
              fontSize := 9.px,
              color := "#888",
              paddingBottom := 6
            ),
          ),
          td(
            if (colType == "double") color := "#49a523" else (),
            v
          ),
          td(c)
        )
      }
    ).render
  }

  def _separator: HR =
    hr(
      margin := 0,
      borderColor := "#a6a6a6"
    ).render

  def _groupedTable(tableBody: TableSection): Table =
    table(
      cls := "tableGrouped",
      tableBody
    ).render


  def _groupedTableBody(colIndex: Int): TableSection =
    tbody(
      id := s"grouped-table-$colIndex"
    ).render

  def _headRow(
    colType: String,
    count: Int,
    sort: String => () => Unit,
    ordering: Int
  ): TableRow = {
    val (sortV, sortC) = ordering match {
      case 1 => (_noSort(colType), _sort(colType, s"bottom"))
      case 2 => (_noSort(colType), _sort(colType, s"top"))
      case 3 => (_sort(colType, s"top"), _noSort(colType))
      case 4 => (_sort(colType, s"bottom"), _noSort(colType))
    }
    tr(
      td(count),
      td(sortV, onclick := sort("value")),
      td(sortC, onclick := sort("count")),
    ).render
  }

  def _sort(colType: String, dir: String): TypedTag[Span] =
    _sortIcon(s"oi oi-caret-$dir", 0)(
      marginTop := -1,
      if (colType == "double") float.right else float.left
    )

  def _noSort(colType: String): TypedTag[Span] =
    span(
      cls := s"oi oi-elevator",
      if (colType == "double") float.right else (),
      color := "#bbbbbb",
      marginTop := 2,
    )

  def _rowStr(
    colIndex: Int,
    n: Int,
    curV: String,
    count: Int,
    update: Int => () => Unit,
    toggle: (Int, String, Int) => () => Unit
  ): TableRow =
    tr(
      id := s"grouped-row-$colIndex-$n",
      td(n),
      td(
        id := s"grouped-cell-$colIndex-$n",
        contenteditable := true,
        onblur := update(n),
        _str2frags(curV)
      ),
      td(
        count,
        onclick := toggle(n, curV, count)
      )
    ).render

  def _rowNum(
    colIndex: Int,
    n: Int,
    curV: String,
    count: Int,
    update: Int => () => Unit,
    toggle: (Int, String, Int) => () => Unit
  ): TableRow =
    tr(
      id := s"grouped-row-$colIndex-$n",
      td(n),
      td(
        id := s"grouped-cell-$colIndex-$n",
        textAlign.right,
        color := "#49a523",
        onblur := update(n),
        contenteditable := true,
        curV,
      ),
      td(
        count,
        onclick := toggle(n, curV, count)
      )
    ).render
}
