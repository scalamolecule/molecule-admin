package moleculeadmin.client.app.element.query

import moleculeadmin.client.app.element.query.datatable.HeadElements
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Element, HR, Span, Table, TableCell, TableRow, TableSection}
import org.scalajs.dom.window
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.JsDom.styles2.wordBreak


trait GroupedAttrElements extends SubMenuElements with HeadElements {

  def _groupedCard(
    header: String,
    grouped: Span,
    values: Table
  ): TypedTag[Element] =
    _card(
      _cardHeader(h5(header)),
      _cardBody(
        padding := 0,
        grouped,
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
            if(colType == "double") color := "#49a523" else (),
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

  def _groupedTableBody(colType: String, count: Int): TableSection =
    tbody(
      tr(
        td(count),
        td(
          span(
            cls := "oi oi-elevator",
            if (colType == "double") float.right else (),
            color := "#bbbbbb"
          ),
        ),
        td(
          _sortIcon("oi oi-caret-bottom", 0)
        ),
      )
    ).render


  def _rowMaker(
    colIndex: Int,
    colType: String,
    attrType: String,
    update: (Int, String, Int) => () => Unit,
    toggle: (Int, String, Int) => () => Unit,
  ): (Int, (String, Int)) => TableRow = {
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


  def _rowStr(
    colIndex: Int,
    n: Int,
    curV: String,
    count: Int,
    update: (Int, String, Int) => () => Unit,
    toggle: (Int, String, Int) => () => Unit
  ): TableRow =
    tr(
      id := s"grouped-row-$colIndex-$n",
      td(n),
      td(
        id := s"grouped-cell-$colIndex-$n",
        contenteditable := true,
        onblur := update(n, curV, count),
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
    update: (Int, String, Int) => () => Unit,
    toggle: (Int, String, Int) => () => Unit
  ): TableRow =
    tr(
      id := s"grouped-row-$colIndex-$n",
      td(n),
      td(
        id := s"grouped-cell-$colIndex-$n",
        textAlign.right,
        color := "#49a523",
        onblur := update(n, curV, count),
        contenteditable := true,
        curV,
      ),
      td(
        count,
        onclick := toggle(n, curV, count)
      )
    ).render
}
