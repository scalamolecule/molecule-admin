package moleculeadmin.client.app.element.query

import moleculeadmin.client.app.element.query.datatable.HeadElements
import org.scalajs.dom.html.{Element, Table}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{h5, _}


trait GroupedAttrElements extends SubMenuElements with HeadElements {

  def _grouped(
    header: String,
    frags: Frag*
  ): TypedTag[Element] =
    _card(
      _cardHeader(h5(header)),
      _cardBody(frags)
    )

  def _groupedTable[T](
    colType: String,
    mandatory: Boolean,
    data: Seq[(T, Int)]
  ): TypedTag[Table] = {
    var i = if (mandatory) 0 else -1
    val headerRow = tr(
      td(),
      td(
        span(
          cls := "oi oi-elevator",
          float.right,
          color := "#bbbbbb"
        ),
        paddingRight := 10
      ),
      td(
        _sortIcon("oi oi-caret-bottom", 0)
      ),
    )

    val valueRows = data.map { case (value, count) =>
      i += 1
      tr(
        cls := "other",
        if (i > 0) {
          td(i, color := "#888")
        } else {
          td()
        },
        td(
          maxWidth := 250,
          colType match {
            case "double" => textAlign.right
            case _        => ()
          },
          value.toString,
          paddingRight := 10,
          //            onclick := useSavedQuery(query)
        ),
        td(
          textAlign.right,
          a(cls := "discrete", href := "#",
            span(
              count,
              paddingBottom := 6
            ),
            //              onclick := retractSavedQuery(query.molecule)
          )
        )
      )
    }

    table(
      cls := "tableGrouped",
      tbody(
        display.block,
        maxHeight := 178,
        overflowY.scroll,
        headerRow +: valueRows
      )
    )
  }
}
