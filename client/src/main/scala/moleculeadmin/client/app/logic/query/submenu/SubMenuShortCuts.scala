package moleculeadmin.client.app.logic.query.submenu

import moleculeadmin.client.app.logic.query.KeyEvents
import moleculeadmin.client.app.html.query.SubMenuElements
import org.scalajs.dom.document
import org.scalajs.dom.html.LI
import rx.Ctx
import scalatags.JsDom.all._
import scala.scalajs.js.timers._


case class SubMenuShortCuts()(implicit val ctx: Ctx.Owner)
  extends SubMenuElements with KeyEvents {


  def render: LI = {
    _subMenuShortcuts(
      _shortCutsTable("Keyboard shortcuts", 14,
        _square("?", "Toggle this shortcuts info", { () => hideShortcuts }),
        _square("q", span("Toggle ", span("Q", textDecoration.underline), "ueries Builder"), { () => toggleQueryBuilder }),
        _square("l", span(span("L", textDecoration.underline), "ist favorite queries"), { () => toggleQueryListMenu() }),
        _square("v", span("Toggle ", span("V", textDecoration.underline), "iews"), { () => toggleViewsMenu() }),
      ),

      _shortCutsTable("Query attribute selections", 14,
        _circle("m", span(span("M", textDecoration.underline), "inimize query to selected attributes"), { () => toggleMinimize }),
        _circle("a", span("Show ", span("A", textDecoration.underline), "ll attributes"), { () => toggleAttrSelectionA }),
        _circle("w", span("Show only attributes ", span("W", textDecoration.underline), "ith values"), { () => toggleAttrSelectionV }),
        _circle("r", span("Show attributes without ", span("R", textDecoration.underline), "ef attributes"), { () => toggleAttrSelectionR }),
      ),

      _shortCutsTable("Data scrolling", 2,
        _square("ctrl-left", "Back", { () => prevPage }),
        _square("ctrl-right", "Forward", { () => nextPage }),
        _square("ctrl-alt-left", "Fast back", { () => prevChunk }),
        _square("ctrl-alt-right", "Fast forward", { () => nextChunk }),
        _square("ctrl-alt-cmd-left", "Start", { () => firstPage }),
        _square("ctrl-alt-cmd-right", "End", { () => lastPage })
      )
    ).render
  }


  def hideShortcuts: SetTimeoutHandle = {
    val el    = document.getElementById("submenu-shortcuts")
    val style = el.getAttribute("style")
    if (style.endsWith("display:block;")) {
      // make sure element is really turned off
      el.setAttribute("style", style.dropRight(14))
    }
    // hide by switching to a non-hover class
    el.setAttribute("class", "dropdown-menu-off")
    setTimeout(200) {
      // re-instate original hover class once temporary class is gone
      el.setAttribute("class", "dropdown-menu")
    }
  }
}
