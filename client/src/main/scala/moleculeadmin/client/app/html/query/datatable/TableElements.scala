package moleculeadmin.client.app.html.query.datatable

import moleculeadmin.client.app.html.AppElements
import org.scalajs.dom.html.{Div, Span, Table}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


trait TableElements extends AppElements {

  def _dataTableContainer: TypedTag[Div] = _rowColAuto(
    id := "dataTableContainer",
    paddingRight := 0
  )

  def _dataTable: TypedTag[Table] = table(cls := "tableData")

  def _buildQueryOpen: TypedTag[Span] = span(paddingTop := 4)

  def _buildQueryClosed: TypedTag[Span] =
    span(
      paddingTop := 4,
      paddingLeft := 25
    )
}
