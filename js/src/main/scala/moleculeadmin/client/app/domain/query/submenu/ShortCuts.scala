package moleculeadmin.client.app.domain.query.submenu
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.document
import rx.{Ctx, Rx}
import scala.scalajs.js.timers._


case class ShortCuts()(implicit val ctx: Ctx.Owner)
  extends RxBindings with SubMenuElements with KeyEvents {

  def hideShortcuts = {
    val el = document.getElementById("submenu-shortcuts")
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


  def dynRender = Rx {
    _subMenuShortcuts(
      _shortCutsTable("Keyboard shortcuts", 14,
        _square("?", "Toggle this shortcuts info", { () => hideShortcuts }),
        _square("q", "Toggle Queries menu", { () => toggleQueriesMenu }),
        _square("r", "Toggle Recent queries menu", { () => toggleRecentMenu }),
        _square("v", "Toggle views", { () => toggleViews }),
        _square("b", "Toggle Query Builder", { () => toggleQueryBuilder }),
        _square("m", "Minimize query to selected attributes", { () => toggleMinimize })
      ),

      _shortCutsTable("Attribute selections", 14,
        _circle("a", "Show all attributes", { () => toggleAttrSelectionA }),
//        _circle("r", "Show attributes without ref attributes", { () => toggleAttrSelectionR }),
//        _circle("v", "Show only attributes with values", { () => toggleAttrSelectionV })
      ),

      _shortCutsTable("Data scrolling", 2,
        _square("ctrl-left", "Back", { () => prevPage }),
        _square("ctrl-right", "Forward", { () => nextPage }),
        _square("ctrl-alt-left", "Fast back", { () => prevChunk }),
        _square("ctrl-alt-right", "Fast forward", { () => nextChunk }),
        _square("ctrl-alt-cmd-left", "Start", { () => firstPage }),
        _square("ctrl-alt-cmd-right", "End", { () => lastPage })
      )
    )
  }
}
