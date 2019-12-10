package moleculeadmin.client.app.element.query
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.jsdom.DropdownMenu
import moleculeadmin.shared.ast.query.QueryData
import org.scalajs.dom.html._
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


trait SubMenuElements extends AppElements with DropdownMenu {


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


  def _subMenuQueryList(
    curMolecule: String,
    newFav: Seq[QueryData],
    queriesByPartNs: Seq[(String, Seq[(String, Seq[(String, QueryData)])])],
    recentQueries: Seq[QueryData],
    savedQueries: Seq[QueryData],
    favoriteQueries: Seq[QueryData],
    savedMolecules: Seq[String],
    favoriteMolecules: Seq[String],
    use: QueryData => () => Unit,
    save: QueryData => () => Unit,
    favorite: QueryData => () => Unit,
    unfavorite: QueryData => () => Unit,
    retract: QueryData => () => Unit,
  ): TypedTag[LI] = {
    li(
      cls := "dropdown",
      a(href := "#", span("L", textDecoration.underline), "ist"),
      div(
        cls := "dropdown-menu",
        id := "submenu-query-list",
        minWidth := 180,
        paddingTop := 8,
        paddingBottom := 5,

        if (recentQueries.nonEmpty)
          li(cls := "dropdown-submenu",
            "Recent queries",
            ul(
              cls := "dropdown-menu",
              li(
                _recentQueries(
                  curMolecule,
                  recentQueries,
                  savedQueries,
                  favoriteQueries,
                  savedMolecules,
                  favoriteMolecules,
                  use,
                  save,
                  favorite
                )
              ),
            )
          ) else (),

        if (savedQueries.nonEmpty)
          li(cls := "dropdown-submenu",
            "Saved queries",
            _savedQueries(
              curMolecule,
              queriesByPartNs,
              favoriteQueries,
              favoriteMolecules,
              use,
              favorite,
              unfavorite,
              retract
            )
          ) else (),

        if (favoriteQueries.nonEmpty || newFav.nonEmpty)
          li(
            paddingTop := 5,
            _favoriteQueries(
              curMolecule,
              newFav,
              favoriteQueries,
              use,
              save,
              unfavorite
            ),
          ) else ()
      )
    )
  }


  def _recentQueries(
    curMolecule: String,
    recentQueries: Seq[QueryData],
    savedQueries: Seq[QueryData],
    favoriteQueries: Seq[QueryData],
    savedMolecules: Seq[String],
    favoriteMolecules: Seq[String],
    use: QueryData => () => Unit,
    save: QueryData => () => Unit,
    favorite: QueryData => () => Unit
  ): TypedTag[Table] = {
    table(
      cls := "tableRowLink",
      recentQueries.map { q =>
        val cur = q.molecule == curMolecule
        tr(
          cls := (if (cur) "current" else "other"),

          td(
            q.molecule,
            paddingRight := 20,
            if (cur) () else onclick := use(q)
          ),

          if (savedMolecules.contains(q.molecule))
            td("")
          else
            td(
              textAlign.right,
              a(cls := "discrete", href := "#", "save", onclick := save(q))
            ),

          if (favoriteMolecules.contains(q.molecule))
            td("")
          else
            td(
              textAlign.right,
              a(cls := "discrete", href := "#", "fav", onclick := favorite(q))
            ),
        )
      }
    )
  }

  def _savedQueriesNs(
    curMolecule: String,
    queriesByNs: Seq[(String, Seq[(String, QueryData)])],
    favoriteQueries: Seq[QueryData],
    favoriteMolecules: Seq[String],
    use: QueryData => () => Unit,
    favorite: QueryData => () => Unit,
    unfavorite: QueryData => () => Unit,
    retract: QueryData => () => Unit,
  ): Seq[TypedTag[LI]] = {
    queriesByNs.map { case (ns, mm) =>
      li(cls := "dropdown-submenu",
        ns,
        ul(
          cls := "dropdown-menu",
          li(
            table(cls := "tableRowLink",
              mm.map { case (m, q) =>
                val cur = m == curMolecule
                tr(
                  cls := (if (cur) "current" else "other"),

                  td(
                    m,
                    paddingRight := 20,
                    if (cur) () else onclick := use(q)
                  ),

                  td(
                    textAlign.right,
                    if (favoriteMolecules.contains(m))
                      a(cls := "discrete", href := "#",
                        "unfav", onclick := unfavorite(q))
                    else
                      a(cls := "discrete", href := "#",
                        "fav", onclick := favorite(q))
                  ),

                  td(
                    textAlign.right,
                    span(
                      cls := "discrete",
                      span(cls := "oi oi-x", fontSize := 9.px,
                        color := "#888", paddingBottom := 6),
                      onclick := retract(q)
                      //                      onclick := { () =>
                      //                        println("hej")
                      //                      }
                    )
                  )
                )
              }
            )
          )
        )
      )
    }
  }

  def _savedQueries(
    curMolecule: String,
    queriesByPartNs: Seq[(String, Seq[(String, Seq[(String, QueryData)])])],
    favoriteQueries: Seq[QueryData],
    favoriteMolecules: Seq[String],
    use: QueryData => () => Unit,
    favorite: QueryData => () => Unit,
    unfavorite: QueryData => () => Unit,
    retract: QueryData => () => Unit,
  ): TypedTag[UList] = {
    ul(
      cls := "dropdown-menu",
      if (queriesByPartNs.head._1 == "db.part/user") {
        // No custom partitions - show nss directly
        _savedQueriesNs(
          curMolecule,
          queriesByPartNs.head._2,
          favoriteQueries,
          favoriteMolecules,
          use,
          favorite,
          unfavorite,
          retract,
        )
      } else {
        queriesByPartNs.map { case (part, queriesByNs) =>
          li(cls := "dropdown-submenu",
            part,
            ul(
              cls := "dropdown-menu",
              _savedQueriesNs(
                curMolecule,
                queriesByNs,
                favoriteQueries,
                favoriteMolecules,
                use,
                favorite,
                unfavorite,
                retract,
              )
            )
          )
        }
      }
    )
  }

  def _favoriteQueries(
    curMolecule: String,
    newFav: Seq[QueryData],
    favoriteQueries: Seq[QueryData],
    use: QueryData => () => Unit,
    save: QueryData => () => Unit,
    unfavorite: QueryData => () => Unit
  ): TypedTag[Table] = {
    table(
      cls := "tableRowLink",
      newFav.map(query =>
        tr(cls := "current",
          th(0),
          td(curMolecule, paddingRight := 20),
          td(
            textAlign.right,
            a(cls := "discrete", href := "#", "save", onclick := save(query))
          )
        )
      ) ++ favoriteQueries.zipWithIndex.map {
        case (query@QueryData(`curMolecule`, _, _, _, _, _, _), i) =>
          tr(cls := "current",
            th(i + 1),
            td(curMolecule, paddingRight := 20),
            td(
              textAlign.right,
              a(cls := "discrete", href := "#",
                span(cls := "oi oi-x", fontSize := 9.px, color := "#888", paddingBottom := 6),
                onclick := unfavorite(query)
              )
            )
          )
        case (query, i)                                            =>
          tr(cls := "other",
            th(i + 1, onclick := use(query)),
            td(query.molecule, paddingRight := 20, onclick := use(query)),
            td(
              textAlign.right,
              a(cls := "discrete", href := "#",
                span(cls := "oi oi-x", fontSize := 9.px, color := "#888", paddingBottom := 6),
                onclick := unfavorite(query)
              )
            )
          )
      }
    )
  }


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
