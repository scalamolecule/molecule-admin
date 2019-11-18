package moleculeadmin.client.app.element.schema
import moleculeadmin.client.app.element.AppElements
import org.scalajs.dom.html.{Div, LI, UList}
import scalatags.JsDom
import scalatags.JsDom.all._


trait SubMenuElements extends AppElements {


  def tabs: JsDom.TypedTag[UList] = ul(cls := "nav nav-tabs")
  def tab(i: Int, label: String, active: Boolean = false): JsDom.TypedTag[LI] = li(
    a(
      attr("data-toggle") := "tab",
      if (active) cls := "nav-pills active py-1" else cls := "nav-pills passive py-1",
      href := s"#tab$i", label
    )
  )
  def tabContent: JsDom.TypedTag[Div] = div(cls := "tab-content")
  def tabPane(i: Int, active: Boolean = false): JsDom.TypedTag[Div] = div(
    id := s"tab$i",
    if (active) cls := "tab-pane active" else cls := "tab-pane"
  )

}
