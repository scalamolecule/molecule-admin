package moleculeadmin.client.app.domain.query.submenu
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.document
import org.scalajs.dom.html.LI
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.scalajs.js.timers._


case class ShortCuts()(implicit val ctx: Ctx.Owner)
  extends RxBindings with SubMenuElements with KeyEvents {

  def hideShortcuts: SetTimeoutHandle = {
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


  def dynRender: Rx.Dynamic[JsDom.TypedTag[LI]] = Rx {
    _subMenuShortcuts(
      _shortCutsTable("Keyboard shortcuts", 14,
        _square("?", "Toggle this shortcuts info", { () => hideShortcuts }),
        _square("q", span("Toggle ", span("Q", textDecoration.underline), "ueries Builder"), { () => toggleQueryBuilder }),
        _square("l", span(span("L", textDecoration.underline), "ist favorite queries"), { () => toggleQueryList }),
//        _square("r", "Toggle Recent queries menu", { () => toggleRecentMenu }),
        _square("v", span("Toggle ", span("V", textDecoration.underline), "iews"), { () => toggleViews }),
      ),

      _shortCutsTable("Query attribute selections", 14,
        _circle("m", span(span("M", textDecoration.underline), "inimize query to selected attributes"), { () => toggleMinimize }),
        _circle("a", span("Show ", span("A", textDecoration.underline), "ll attributes"), { () => toggleAttrSelectionA }),
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
