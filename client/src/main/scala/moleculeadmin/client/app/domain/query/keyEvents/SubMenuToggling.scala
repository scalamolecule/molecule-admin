package moleculeadmin.client.app.domain.query.keyEvents

import molecule.util.RegexMatching
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{KeyboardEvent, document, window}
import rx.Ctx


trait SubMenuToggling extends BaseKeyEvents with RegexMatching {

  def toggle(id: String): Unit = {
    val el = document.getElementById("submenu-" + id)
    if (el != null) {
      val style = el.getAttribute("style")
      if (style.endsWith("display:block;")) {
        // make sure element is really turned off
        el.setAttribute("style", style.dropRight(14))
      } else {
        el.setAttribute("style", style + "display:block;")
      }
    }
  }

  def toggleOffElement(id: String): Unit = {
    val el = document.getElementById("submenu-" + id)
    if (el != null) {
      val style = el.getAttribute("style")
      if (style.endsWith("display:block;")) {
        // make sure element is really turned off
        el.setAttribute("style", style.dropRight(14))
      }
    }
  }

  def toggleOffAll(): Unit = {
    toggleOffElement("query-list")
    toggleOffElement("views")
    toggleOffElement("grouped")
    toggleOffElement("shortcuts")
  }
  def toggleQueryListMenu(): Unit = {
    toggle("query-list")
    toggleOffElement("views")
    toggleOffElement("grouped")
    toggleOffElement("shortcuts")
  }
  def toggleViewsMenu(): Unit = {
    toggleOffElement("query-list")
    toggle("views")
    toggleOffElement("grouped")
    toggleOffElement("shortcuts")
  }
  def toggleGroupedMenu(): Unit = {
    toggleOffElement("query-list")
    toggleOffElement("views")
    toggle("grouped")
    toggleOffElement("shortcuts")
  }
  def toggleShortcutsMenu(): Unit = {
    toggleOffElement("query-list")
    toggleOffElement("views")
    toggleOffElement("grouped")
    toggle("shortcuts")
  }


  // Query list ------------------------------

  def queryListOpen: Boolean = {
    val element = document.getElementById("submenu-query-list")
    element != null && element.getAttribute("style").endsWith("display:block;")
  }

  def queryList(e: KeyboardEvent, key: String)(implicit ctx: Ctx.Owner): Unit =
    key match {
      case " " if curMolecule.now.nonEmpty => upsertCurrentQuery(e)
      case r"\d"                           =>
        if (savedQueries.count(_.isFavorite) < 10)
          favoriteQuery(key.toInt - 1)
        else
          numberInputs(key, favoriteQuery)
      case _                               => ()
    }

  def favoriteQuery(i: Int)(implicit ctx: Ctx.Owner): Unit = {
    val favoriteQueries = savedQueries.filter(_.isFavorite)
    if (i < favoriteQueries.size) {
      (new Callbacks).useQuery(
        favoriteQueries.sortBy(_.molecule).apply(i)
      )
    } else {
      window.alert(s"Only ${favoriteQueries.size} queries saved as favorites.")
    }
  }

  def upsertCurrentQuery(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent default scroll to bottom
    e.preventDefault()
    val callback = new Callbacks
    callback.upsertQuery(callback.curQuery, true)
  }


  // Grouped ------------------------------

  def groupedOpen: Boolean = {
    val element = document.getElementById("submenu-grouped")
    element != null &&
      element.getAttribute("style").endsWith("display:block;") &&
      element.children.length > 1
  }

  def grouped(e: KeyboardEvent, key: String)(implicit ctx: Ctx.Owner): Unit =
    key match {
      case " "   => toggleShowGrouped(e)
      case r"\d" =>
        if (groupableCols.size < 10)
          toggleGrouped(key.toInt - 1)
        else
          numberInputs(key, toggleGrouped)
      case _     => ()
    }

  def toggleShowGrouped(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent default scroll to bottom
    e.preventDefault()
    document.getElementById("checkbox-grouped-showGrouped")
      .asInstanceOf[HTMLInputElement].checked = !showGrouped
    (new Callbacks).toggleShowGrouped()
  }

  def toggleGrouped(i: Int)(implicit ctx: Ctx.Owner): Unit = {
    if (i == -1 || i >= groupableCols.size)
      window.alert(s"Unrecognized shortcut for grouped attribute: $i")
    else
      (new Callbacks).toggleGrouped(groupableCols(i))
  }


  // Views ------------------------------

  def viewsOpen: Boolean = {
    val element = document.getElementById("submenu-views")
    element != null && element.getAttribute("style").endsWith("display:block;")
  }

  def views(e: KeyboardEvent, key: String)(implicit ctx: Ctx.Owner): Unit =
    key match {
      case " "   => toggleShowViews(e)
      case r"\d" => numberInputs(key, toggleView)
      case _     => ()
    }

  def toggleShowViews(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent default scroll to bottom
    e.preventDefault()
    document.getElementById("checkbox-view-showViews")
      .asInstanceOf[HTMLInputElement].checked = !showViews
    (new Callbacks).toggleShowViews()
  }

  def toggleView(i: Int)(implicit ctx: Ctx.Owner): Unit = {
    if (i == -1 || i >= allViews.size)
      window.alert(s"Unrecognized shortcut for view: $i")
    else
      (new Callbacks).toggleView(allViews(i)._1)
  }
}
