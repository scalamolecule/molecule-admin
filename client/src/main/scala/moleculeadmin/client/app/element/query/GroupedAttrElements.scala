package moleculeadmin.client.app.element.query

import moleculeadmin.client.app.element.query.datatable.HeadElements
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Element, HR, Span, Table, TableCell, TableRow, TableSection}
import org.scalajs.dom.window
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{h5, tr, _}


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
    vs: Seq[(Int, String)],
    toggleOff: (Int, String) => Unit
  ): Table = {
    table(
      cls := "tableGroupedSelected",
      vs.map { case (n, v) =>
        tr(
          onclick := { () => toggleOff(n, v) },
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
          td(v)
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
    colType: String,
    attrType: String,
    update: String => () => Unit,
    toggle: (Int, String) => () => Unit,
  ): (Int, (String, Int)) => TableRow = {
    if (colType == "double") {
      (i: Int, row: (String, Int)) =>
        _rowNum(i, row._1, row._2, update, toggle)
    } else {
      attrType match {
        case "String" =>
          (i: Int, row: (String, Int)) =>
            _rowStr(i, row._1, row._2, update, toggle)
      }
    }
  }


  def _rowStr(
    n: Int,
    curV: String,
    count: Int,
    update: String => () => Unit,
    toggle: (Int, String) => () => Unit
  ): TableRow =
    tr(
      id := s"grouped-$n",
      td(n),
      td(
        onblur := update(curV),
        contenteditable := true,
        _str2frags(curV)
      ),
      td(
        count,
        onclick := toggle(n, curV)
      )
    ).render

  def _rowNum(
    n: Int,
    curV: String,
    count: Int,
    update: String => () => Unit,
    toggle: (Int, String) => () => Unit
  ): TableRow =
    tr(
      td(n),
      td(
        textAlign.right,
        onblur := update(curV),
        contenteditable := true,
        curV,
      ),
      td(
        count,
        onclick := toggle(n, curV)
      )
    ).render
}
