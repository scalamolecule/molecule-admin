package moleculeadmin.client.app.html.query

import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.app.html.common.DropdownMenu
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Div, UList}
import org.scalajs.dom.window
import scalatags.JsDom
import scalatags.JsDom.all._

trait SchemaDropdownElements extends AppElements with DropdownMenu {

  override def _topMenu: JsDom.TypedTag[UList] = ul(cls := "dropdown-menu level1 corner")

  def _selection(selection: String): JsDom.TypedTag[Div] = div(
    color := Color.textGray,
    fontSize := "11px",
    marginTop := 6,
    paddingLeft := 10,
    selection,
    cursor.pointer,
    onclick := { () =>
      window.alert(
        "Various attribute selections can be toggle with shortcuts:\n" +
          "a - All attributes\n" +
          "w - Attributes with values\n" +
          "r - Attributes except ref attributes"
      )
    }
  )
}
