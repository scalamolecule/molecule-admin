package moleculeadmin.client.jsdom

import org.scalajs.dom.html.{Div, LI, UList}
import scalatags.JsDom
import scalatags.JsDom.all._

trait DropdownMenu {

  def _topMenu: JsDom.TypedTag[UList] = ul(cls := "dropdown-menu level1")

  def _menu: JsDom.TypedTag[UList] = ul(cls := "dropdown-menu")
  def _menu(clazz: String): JsDom.TypedTag[UList] = ul(cls := s"dropdown-menu $clazz")

  def _submenu: JsDom.TypedTag[LI] = li(cls := "dropdown-submenu")
  def _submenu(clazz: String): JsDom.TypedTag[LI] = li(cls := s"dropdown-submenu $clazz")

  def _divider: JsDom.TypedTag[Div] = div(cls := "dropdown-divider")

}
