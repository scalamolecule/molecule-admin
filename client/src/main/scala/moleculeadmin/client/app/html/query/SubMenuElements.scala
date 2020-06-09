package moleculeadmin.client.app.html.query

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.app.html.common.DropdownMenu
import moleculeadmin.shared.ast.query.QueryDTO
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html._
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.generic


trait SubMenuElements extends AppElements with DropdownMenu {

  // Max row selector --------------------------------------------------------

  val _maxRowsSelector =
    select(
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


  // Queries -------------------------------------------------------------------

  def _subMenuQueryList(
    curMolecule: String,
    newFav: Seq[QueryDTO],
    queriesByPartNs: Seq[(String, Seq[(String, Seq[(String, QueryDTO)])])],
    recentQueries: Seq[QueryDTO],
    savedQueries: Seq[QueryDTO],
    favoriteQueries: Seq[QueryDTO],
    savedMolecules: Seq[String],
    favoriteMolecules: Seq[String],
    use: QueryDTO => () => Unit,
    upsert: QueryDTO => () => Unit,
    favorite: QueryDTO => () => Unit,
    unfavorite: QueryDTO => () => Unit,
    retract: QueryDTO => () => Unit,
  ): TypedTag[LI] =
    li(
      cls := "dropdown",
      a(href := "#", _shortcut("L", "ist")),
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
                  upsert,
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
              upsert,
              unfavorite
            ),
          ) else ()
      )
    )

  def _recentQueries(
    curMolecule: String,
    recentQueries: Seq[QueryDTO],
    savedQueries: Seq[QueryDTO],
    favoriteQueries: Seq[QueryDTO],
    savedMolecules: Seq[String],
    favoriteMolecules: Seq[String],
    use: QueryDTO => () => Unit,
    save: QueryDTO => () => Unit,
    favorite: QueryDTO => () => Unit
  ): TypedTag[Table] =
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

  def _savedQueriesNs(
    curMolecule: String,
    queriesByNs: Seq[(String, Seq[(String, QueryDTO)])],
    favoriteQueries: Seq[QueryDTO],
    favoriteMolecules: Seq[String],
    use: QueryDTO => () => Unit,
    favorite: QueryDTO => () => Unit,
    unfavorite: QueryDTO => () => Unit,
    retract: QueryDTO => () => Unit
  ): Seq[TypedTag[LI]] =
    queriesByNs.map { case (ns, mm) =>
      li(cls := "dropdown-submenu",
        ns,
        ul(
          cls := "dropdown-menu",
          li(
            table(cls := "tableRowLink",
              mm.zipWithIndex.map { case ((m, q), i) =>
                val cur   = m == curMolecule
                // Toggle favorite status
                var isFav = favoriteMolecules.contains(m)
                def action: Unit = {
                  val elem = document.getElementById("savedFav" + i)
                  elem.innerText = ""
                  elem.appendChild(if (isFav) "fav".render else "unfav".render)
                  if (isFav) unfavorite(q)() else favorite(q)()
                  isFav = !isFav
                }

                tr(
                  id := "saved" + i,
                  cls := (if (cur) "current" else "other"),
                  td(
                    textAlign.right,
                    span(
                      cls := "discrete",
                      span(cls := "oi oi-x", fontSize := 9.px,
                        color := "#888", paddingBottom := 6),
                      onclick := { () =>
                        document.getElementById("saved" + i).innerText = ""
                        retract(q)()
                      }
                    )
                  ),
                  td(
                    textAlign.right,
                    a(
                      cls := "discrete", href := "#",
                      span(
                        id := "savedFav" + i,
                        if (isFav) "unfav" else "fav",
                      ),
                      onclick := { () => action }
                    )
                  ),
                  td(m, paddingRight := 20, if (cur) () else onclick := use(q)),
                )
              }
            )
          )
        )
      )
    }

  def _savedQueries(
    curMolecule: String,
    queriesByPartNs: Seq[(String, Seq[(String, Seq[(String, QueryDTO)])])],
    favoriteQueries: Seq[QueryDTO],
    favoriteMolecules: Seq[String],
    use: QueryDTO => () => Unit,
    favorite: QueryDTO => () => Unit,
    unfavorite: QueryDTO => () => Unit,
    retract: QueryDTO => () => Unit
  ): TypedTag[UList] =
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
          retract
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
                retract
              )
            )
          )
        }
      }
    )

  def _favoriteQueryRows(
    curMolecule: String,
    newFav: Seq[QueryDTO],
    favoriteQueries: Seq[QueryDTO],
    use: QueryDTO => () => Unit,
    upsert: QueryDTO => () => Unit,
    unfavorite: QueryDTO => () => Unit
  ): Seq[TypedTag[TableRow]] = {
    newFav.map(query =>
      tr(
        cls := "other",
        onclick := upsert(query),
        th("␣", whiteSpace.nowrap),
        td("save current query...",
          whiteSpace.nowrap,
          paddingRight := 20,
          colspan := 2
        )
      )
    ) ++ favoriteQueries.zipWithIndex.map { case (q, i) =>
      val cur = q.molecule == curMolecule

      val action: generic.AttrPair[dom.Element, () => Unit] =
        onclick := (if (cur) () => () else use(q))

      val favoriteId = "favorite" + i

      if (cur) {
        tr(
          id := favoriteId,
          cls := "current",
          th(i + 1),
          td(
            textAlign.left,
            a(
              href := "#",
              "unfav",
              onclick := { () =>
                document.getElementById(favoriteId).innerText = ""
                unfavorite(q)()
              }
            )
          ),
          td(q.molecule)
        )
      } else {
        tr(
          id := favoriteId,
          cls := "other",
          th(i + 1),
          td(
            textAlign.left,
            a(
              href := "#",
              "unfav",
              onclick := { () =>
                document.getElementById(favoriteId).innerText = ""
                unfavorite(q)()
              }
            )
          ),
          td(
            a(
              href := "#",
              q.molecule,
              action
            )
          )
        )
      }
    }
  }

  def _favoriteQueries(
    curMolecule: String,
    newFav: Seq[QueryDTO],
    favoriteQueries: Seq[QueryDTO],
    use: QueryDTO => () => Unit,
    upsert: QueryDTO => () => Unit,
    unfavorite: QueryDTO => () => Unit
  ): TypedTag[Table] =
    table(
      id := "querylist-favorites",
      cls := "tableRowLink",
      _favoriteQueryRows(
        curMolecule,
        newFav,
        favoriteQueries,
        use,
        upsert,
        unfavorite
      )
    )


  // Grouped -------------------------------------------------------------------

  def _subMenu(
    idStr: String,
    header: Frag,
    checkboxes: Seq[TypedTag[HTMLElement]],
  ): TypedTag[LI] =
    li(cls := "dropdown",
      a(href := "#", header),
      div(
        id := idStr,
        cls := "dropdown-menu",
        minWidth := 200,
        paddingBottom := 3,
        checkboxes
      )
    )


  // Shortcuts -----------------------------------------------------------------

  def _subMenuShortcuts(shortcutTables: Frag*): TypedTag[LI] =
    li(
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

  def _shortCutsTable(header: String, p: Int, shortcuts: Frag*): TypedTag[Span] =
    span(
      h5(header, paddingBottom := 10),
      table(cls := "shortcuts",
        marginBottom := p,
        shortcuts)
    )

  def _square(key: String, label: Frag, onclck: () => Unit): TypedTag[TableRow] =
    tr(
      cls := "other",
      th(span(key, cls := "box")),
      td(label),
      onclick := onclck
    )

  def _circle(key: String, label: Frag, onclck: () => Unit): TypedTag[TableRow] =
    tr(
      cls := "other",
      th(span(cls := "circle", key)),
      td(label),
      onclick := onclck
    )

}
