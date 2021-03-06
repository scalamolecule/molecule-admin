package moleculeadmin.client.app.html.query

import moleculeadmin.client.app.css.Color
import moleculeadmin.client.app.html.query.datatable.HeadElements
import org.scalajs.dom.html._
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{marginTop, _}


trait GroupedAttrElements extends SubMenuElements with HeadElements {

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

  def _groupedSelected[T](
    colType: String,
    vs: Seq[(Int, scala.Option[T], Int)],
    toggleOff: (Int, scala.Option[T], Int) => Unit
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
            if (colType == "double" || colType == "listDouble")
              color := "#49a523" else (),
            v.fold("")(_.toString)
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
    span(
      if (colType == "double" || colType == "listDouble") float.right else (),
      span(cls := s"oi oi-caret-$dir", verticalAlign.middle,
        paddingLeft := 0,
      ),
      color := Color.icon,
      whiteSpace.nowrap,
      cursor.pointer,
    )

  def _noSort(colType: String): TypedTag[Span] =
    span(
      cls := s"oi oi-elevator",
      if (colType == "double" || colType == "listDouble") float.right else (),
      color := "#bbbbbb",
      marginTop := 1,
    )

  def _rowStr(
    rowId: String,
    cellId: String,
    rowIndex: Int,
    curVopt: scala.Option[String],
    count: Int,
    update: () => Unit,
    toggle: () => Unit
  ): TableRow =
    tr(
      id := rowId,
      td(rowIndex + 1),
      td(
        id := cellId,
        contenteditable := true,
        onblur := update,
        _optStr2frags(curVopt)
      ),
      td(
        count,
        onclick := toggle
      )
    ).render

  def _rowNum(
    rowId: String,
    cellId: String,
    rowIndex: Int,
    curV: String,
    count: Int,
    update: () => Unit,
    toggle: () => Unit
  ): TableRow =
    tr(
      id := rowId,
      td(rowIndex + 1),
      td(
        id := cellId,
        textAlign.right,
        color := "#49a523",
        onblur := update,
        contenteditable := true,
        curV,
      ),
      td(
        count,
        onclick := toggle
      )
    ).render
}
