package moleculeadmin.client.app.html.common
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Anchor, Div, Select, Span}
import scalatags.JsDom
import scalatags.JsDom.all._

trait TopMenuElements extends AppElements {

  val _topBar: JsDom.TypedTag[Div] = _containerFluid(
    padding := "4px 5px 4px 13px",
    backgroundColor := "#dfdfdf",
    borderBottomStyle.solid,
    borderBottomWidth := 1.px,
    borderBottomColor := "#b5b5b5",
  )

  def _logo(src1: String, href1: String): JsDom.TypedTag[Anchor] = a(
    href := "/",
    img(
      src := src1,
      height := 18.px,
      marginTop := (-1).px,
      marginRight := 12.px,
    )
  )

  def _dbSelector(select1: Select): JsDom.TypedTag[Span] = span(
    select1,
    paddingRight := 16.px
  )


  def _pageLink(page: String, href1: String): JsDom.TypedTag[Anchor] = a(
    cls := "discrete",
    href := href1,
    page,
    paddingRight := 12.px
  )

  def _curPageLink(page: String, href1: String): JsDom.TypedTag[Anchor] =
    _pageLink(page, href1)(
      fontWeight.bold,
      color.black
    )


  def _space: JsDom.TypedTag[Span] = span(paddingRight := 40.px)


  def _viewLink(view: String): JsDom.TypedTag[Anchor] = a(
    cls := "discrete",
    href := "#",
    fontStyle.italic,
    view,
    paddingRight := 12.px
  )

  def _curViewLink(view: String): JsDom.TypedTag[Anchor] =
    _viewLink(view)(color := Color.link)
}
