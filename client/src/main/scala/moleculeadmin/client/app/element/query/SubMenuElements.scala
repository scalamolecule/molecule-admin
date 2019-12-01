package moleculeadmin.client.app.element.query
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.shared.ast.query.SavedQuery
import org.scalajs.dom.html._
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


trait SubMenuElements extends AppElements {


  val _maxRowsSelector = select(
    marginRight := 5,
    for ((v, label) <- Seq(
      (-1, "All"),
      (25, "25"),
      (50, "50"),
      (100, "100"),
      (500, "500"),
      (1000, "1K"),
      (5000, "5K"),
      (10000, "10K"),
      (50000, "50K"),
      (100000, "100K"),
      (500000, "500K"),
      (1000000, "1M")
    )) yield {
      if (v == maxRows.now)
        option(value := v, label, selected)
      else
        option(value := v, label)
    }
  ).render


  // Queries --------------------------------------------------------------------

  def _queriesTable(
    savedQueries: Seq[SavedQuery],
    curMolecule: String,
    useSavedQuery: SavedQuery => () => Unit,
    retractSavedQuery: String => () => Unit
  ): TypedTag[Table] = table(cls := "tableRowLink",
    savedQueries.zipWithIndex.map {
      case (SavedQuery(`curMolecule`, _, _, _), i) => tr(cls := "current",
        th(i + 1),
        td(curMolecule, paddingRight := 20),
        td(
          textAlign.right,
          a(cls := "discrete", href := "#",
            span(cls := "oi oi-x", fontSize := 9.px, color := "#888", paddingBottom := 6),
            onclick := retractSavedQuery(curMolecule)
          )
        )
      )
      case (query, i)                              => tr(cls := "other",
        th(i + 1, onclick := useSavedQuery(query)),
        td(query.molecule, paddingRight := 20, onclick := useSavedQuery(query)),
        td(
          textAlign.right,
          a(cls := "discrete", href := "#",
            span(cls := "oi oi-x", fontSize := 9.px, color := "#888", paddingBottom := 6),
            onclick := retractSavedQuery(query.molecule)
          )
        )
      )
    }
  )

  def _subMenuQueries(
    savedQueries: Seq[SavedQuery],
    curMolecule: String,
    useSavedQuery: SavedQuery => () => Unit,
    retractSavedQuery: String => () => Unit
  ): TypedTag[LI] = li(
    cls := "dropdown",
    a(href := "#", span("Q", textDecoration.underline), "ueries"),
    div(
      cls := "dropdown-menu",
      id := "submenu-queries",
      paddingBottom := 5,
      _queriesTable(savedQueries, curMolecule, useSavedQuery, retractSavedQuery)
    )
  )


  // Recent molecules --------------------------------------------------------------------

  def _recentMoleculesList(
    recentMolecules: Seq[String],
    curMolecule: String,
    useRecentMolecule: String => () => Unit,
    savedMolecules: Seq[String],
    saveQuery: String => () => Unit,
    removeRecentMolecules: String => () => Unit
  ): TypedTag[Table] = table(cls := "tableRowLink",
    recentMolecules.zipWithIndex.map {
      case (m@`curMolecule`, i) => tr(cls := "current",
        th(i + 1),
        td(m, paddingRight := 20),
        if (savedMolecules.contains(m)) td("") else
          td(textAlign.right, a(cls := "discrete", href := "#", "fav", onclick := saveQuery(m))),
        td("")
      )
      case (m, i)               => tr(cls := "other",
        th(i + 1, onclick := useRecentMolecule(m)),
        td(m, paddingRight := 20, onclick := useRecentMolecule(m)),
        if (savedMolecules.contains(m)) td("") else
          td(textAlign.right, a(cls := "discrete", href := "#", "fav", onclick := saveQuery(m))),
        td(
          textAlign.right,
          a(cls := "discrete", href := "#",
            span(cls := "oi oi-x", fontSize := 9.px, color := "#888", paddingBottom := 6),
            onclick := removeRecentMolecules(m)
          )
        )
      )
    }
  )

  def _subMenuRecent(
    recentMolecules: Seq[String],
    curMolecule: String,
    useRecentMolecule: String => () => Unit,
    savedMolecules: Seq[String],
    saveQuery: String => () => Unit,
    removeRecentMolecules: String => () => Unit
  ): TypedTag[LI] = li(
    cls := "dropdown",
    a(href := "#", span("R", textDecoration.underline), "ecent"),
    div(
      cls := "dropdown-menu",
      id := "submenu-recentMolecules",
      paddingBottom := 5,
      _recentMoleculesList(recentMolecules, curMolecule, useRecentMolecule, savedMolecules, saveQuery, removeRecentMolecules)
    )
  )


  // Views --------------------------------------------------------------------

  def _subMenu(header: String, checkboxes: Seq[TypedTag[HTMLElement]]): TypedTag[LI] = li(cls := "dropdown",
    a(href := "#", header),
    div(
      cls := "dropdown-menu",
      width := 200,
      paddingBottom := 3,
      checkboxes
    )
  )

  def _cb(id1: String, txt: Frag, checked1: Boolean, toggle: () => Unit): TypedTag[Div] = div(
    input(tpe := "checkbox",
      id := s"checkbox-$id1",
      value := id1,
      if (checked1) checked := true else (),
      onchange := toggle
    ),
    label(
      paddingLeft := 5,
      marginBottom := 3,
      `for` := s"checkbox-$id1",
      txt
    )
  )


  // Grouped --------------------------------------------------------------------

  def _groupedTable[T](
    data: Seq[(T, Int)]
  ): TypedTag[Table] = {
    var i = 0
    table(
      cls := "tableGrouped",
      height := 500,
      tbody(
        display.block,
        maxHeight := 500,
        overflowY.scroll,
        data.map { case (value, count) =>
          i += 1
          tr(
            cls := "other",
            th(i, color := "#888"),
            td(
              maxWidth := 250,
              value.toString,
              paddingRight := 20,
              //            onclick := useSavedQuery(query)
            ),
            td(
              textAlign.right,
              a(cls := "discrete", href := "#",
                span(
                  count,
                  paddingBottom := 6
                ),
                //              onclick := retractSavedQuery(query.molecule)
              )
            )
          )
        }
      )
    )
  }

  // Shortcuts --------------------------------------------------------------------

  def _subMenuShortcuts(shortcutTables: Frag*): TypedTag[LI] = li(
    cls := "dropdown",
    paddingLeft := 15,
    a(href := "#", i(cls := "far fa-keyboard")),
    div(
      cls := "dropdown-menu",
      id := "submenu-shortcuts",
      paddingBottom := 5,
      width := 270,
      shortcutTables
    )
  )

  def _shortCutsTable(header: String, p: Int, shortcuts: Frag*): TypedTag[Span] = span(
    h5(header, paddingBottom := 10),
    table(cls := "shortcuts",
      marginBottom := p,
      shortcuts)
  )

  def _square(key: String, label: String, onclck: () => Unit): TypedTag[TableRow] = tr(
    cls := "other",
    th(span(key, cls := "box")),
    td(label),
    onclick := onclck
  )

  def _circle(key: String, label: String, onclck: () => Unit): TypedTag[TableRow] = tr(
    cls := "other",
    th(span(cls := "circle", key)),
    td(label),
    onclick := onclck
  )

}
