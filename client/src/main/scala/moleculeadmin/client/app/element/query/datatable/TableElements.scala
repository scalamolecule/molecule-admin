package moleculeadmin.client.app.element.query.datatable
import moleculeadmin.client.app.element.AppElements
import org.scalajs.dom.html.{Div, Table}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


trait TableElements  extends AppElements {

  def _dataTableContainer: TypedTag[Div] = _rowColAuto(
    id := "dataTableContainer",
    paddingRight := 0
  )

  def _dataTable: TypedTag[Table] = table(cls := "tableData")
}
