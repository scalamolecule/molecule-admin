package moleculeadmin.client.app.element.query
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Div, LI, Span, Table, TableRow}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.window
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{table, td, _}
import moleculeadmin.shared.ast.query.Favorite


trait SubMenuElements extends AppElements {


  val _maxRowsSelector = select(
    marginRight := 5,
    Seq(
      option(value := -1, "All"),
      option(value := 25, "25"),
      option(value := 50, "50"),
      option(value := 100, "100"),
      option(value := 500, "500"),
      option(value := 1000, "1K"),
      option(value := 5000, "5K"),
      option(value := 10000, "10K"),
      option(value := 50000, "50K"),
      option(value := 100000, "100K"),
      option(value := 500000, "500K"),
      option(value := 1000000, "1M"),
    )
  ).render


  // Favorites --------------------------------------------------------------------

  def _favoritesTable(favorites: Seq[Favorite],
                      curMolecule: String,
                      useFavCallback: Favorite => () => Unit,
                      retractFavCallback: String => () => Unit
                     ): TypedTag[Table] = table(cls := "tableRowLink",
    favorites.zipWithIndex.map {
      case (Favorite(`curMolecule`, _), i) => tr(cls := "current",
        th(i + 1),
        td(curMolecule, paddingRight := 20),
        td(
          textAlign.right,
          a(cls := "discrete", href := "#",
            span(cls := "oi oi-x", fontSize := 9.px, color := "#888", paddingBottom := 6),
            onclick := retractFavCallback(curMolecule)
          )
        )
      )
      case (favorite, i)                   => tr(cls := "other",
        th(i + 1, onclick := useFavCallback(favorite)),
        td(favorite.molecule, paddingRight := 20, onclick := useFavCallback(favorite)),
        td(
          textAlign.right,
          a(cls := "discrete", href := "#",
            span(cls := "oi oi-x", fontSize := 9.px, color := "#888", paddingBottom := 6),
            onclick := retractFavCallback(favorite.molecule)
          )
        )
      )
    }
  )

  def _subMenuFavorites(favorites: Seq[Favorite],
                        curMolecule: String,
                        useFavCallback: Favorite => () => Unit,
                        retractFavCallback: String => () => Unit
                       ): TypedTag[LI] = li(
    cls := "dropdown",
    a(href := "#", span("F", textDecoration.underline), "avorites"),
    div(
      cls := "dropdown-menu",
      id := "submenu-favorites",
      paddingBottom := 5,
      _favoritesTable(favorites, curMolecule, useFavCallback, retractFavCallback)
    )
  )


  // Cache --------------------------------------------------------------------

  def _cacheList(molecules: Seq[String],
                 curMolecule: String,
                 useCachedCallback: String => () => Unit,
                 curFavMolecules: Seq[String],
                 addFavCallback: String => () => Unit,
                 removeCachedCallback: String => () => Unit
                ): TypedTag[Table] = table(cls := "tableRowLink",
    molecules.zipWithIndex.map {
      case (m@`curMolecule`, i) => tr(cls := "current",
        th(i + 1),
        td(m, paddingRight := 20),
        if (curFavMolecules.contains(m)) td("") else
          td(textAlign.right, a(cls := "discrete", href := "#", "fav", onclick := addFavCallback(m))),
        td("")
      )
      case (m, i)               => tr(cls := "other",
        th(i + 1, onclick := useCachedCallback(m)),
        td(m, paddingRight := 20, onclick := useCachedCallback(m)),
        if (curFavMolecules.contains(m)) td("") else
          td(textAlign.right, a(cls := "discrete", href := "#", "fav", onclick := addFavCallback(m))),
        td(
          textAlign.right,
          a(cls := "discrete", href := "#",
            span(cls := "oi oi-x", fontSize := 9.px, color := "#888", paddingBottom := 6),
            onclick := removeCachedCallback(m)
          )
        )
      )
    }
  )

  def _subMenuCache(molecules: Seq[String],
                    curMolecule: String,
                    useCachedCallback: String => () => Unit,
                    curFavMolecules: Seq[String],
                    addFavCallback: String => () => Unit,
                    removeCachedCallback: String => () => Unit
                   ): TypedTag[LI] = li(
    cls := "dropdown",
    a(href := "#", span("C", textDecoration.underline), "ache"),
    div(
      cls := "dropdown-menu",
      id := "submenu-cache",
      paddingBottom := 5,
      _cacheList(molecules, curMolecule, useCachedCallback, curFavMolecules, addFavCallback, removeCachedCallback)
    )
  )


  // Snippets --------------------------------------------------------------------

  def _subMenuSnippet(checkboxes: TypedTag[HTMLElement]*): TypedTag[LI] = li(cls := "dropdown",
    a(href := "#", "Snippets"),
    div(
      cls := "dropdown-menu",
      width := 200,
      paddingBottom := 3,
      checkboxes
    )
  )

  def _cb(id1: String, txt: Frag, checked1: Boolean, callback: () => Unit): TypedTag[Div] = div(
    input(tpe := "checkbox",
      id := s"checkbox-$id1",
      value := id1,
      if (checked1) checked := true else (),
      onchange := callback
    ),
    label(
      paddingLeft := 5,
      `for` := s"checkbox-$id1",
      txt
    )
  )


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
