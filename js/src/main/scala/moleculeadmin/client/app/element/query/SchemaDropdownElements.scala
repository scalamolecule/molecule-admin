package moleculeadmin.client.app.element.query

import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.jsdom.DropdownMenu
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.jsdom.DropdownMenu
import org.scalajs.dom.html.UList
import scalatags.JsDom
import scalatags.JsDom.all._

trait SchemaDropdownElements extends AppElements with DropdownMenu {


  override def _topMenu: JsDom.TypedTag[UList] = ul(cls := "dropdown-menu level1 corner")


}
