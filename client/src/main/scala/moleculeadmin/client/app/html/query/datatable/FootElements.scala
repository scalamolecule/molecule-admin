package moleculeadmin.client.app.html.query.datatable
import moleculeadmin.client.app.css.Color
import moleculeadmin.client.app.html.AppElements
import org.scalajs.dom.html.{Div, Select, Table}
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


trait FootElements extends AppElements {

  def _limitSelector(curIndex: Int): Select = select(
    marginRight := 12,
    for (i <- Seq(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)) yield {
      if (i == curIndex)
        option(value := i, i, selected)
      else
        option(value := i, i)
    }
  ).render

  def _firstPage(first: Boolean): TypedTag[HTMLElement] = if (first)
    span(cls := "oi oi-media-step-backward", color := Color.iconGray, paddingRight := 10)
  else
    a(cls := "oi oi-media-step-backward", href := "#", color := Color.icon, paddingRight := 10)

  def _prevPage(first: Boolean): TypedTag[HTMLElement] = if (first)
    span(cls := "oi oi-media-skip-backward", color := Color.iconGray, paddingRight := 10)
  else
    a(cls := "oi oi-media-skip-backward", href := "#", color := Color.icon, paddingRight := 10)


  def _nextPage(last: Boolean): TypedTag[HTMLElement] = if (last)
    span(cls := "oi oi-media-skip-forward", color := Color.iconGray, paddingRight := 10)
  else
    a(cls := "oi oi-media-skip-forward", href := "#", color := Color.icon, paddingRight := 10)

  def _lastPage(last: Boolean): TypedTag[HTMLElement] = if (last)
    span(cls := "oi oi-media-step-forward", color := Color.iconGray, paddingRight := 15)
  else
    a(cls := "oi oi-media-step-forward", href := "#", color := Color.icon, paddingRight := 15)

}
