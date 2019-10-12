package moleculeadmin.client.app.element.query
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Div, Element, LI, Span, TableRow}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{h5, _}
import org.scalajs.dom.window
import moleculeadmin.shared.ast.query.Favorite

trait SnippetElements extends SubMenuElements {

  val _avoidAutofocus = input(tpe := "hidden", autofocus := true)

  def _moleculeSnippet(rows1: Int,
                       cols1: Int,
                       moleculeStr: String,
                       addFav: () => Unit,
                       alreadyFav: Boolean
                      ): TypedTag[Element] = _card(
    _cardHeader(
      h5("Molecule", display.`inline-block`),
      if (alreadyFav) {
        span(paddingLeft := 30, float.right, "fav", color := Color.textGray,
          onclick := { () =>
            window.alert(s"`$moleculeStr` is already a favorite molecule.") })
      } else {
        span(paddingLeft := 30, float.right,
          a(cls := "discrete", href := "#", "fav", onclick := addFav))
      }
    ),
    _cardBody(
      textarea(
        margin := 3,
        rows := rows1,
        cols := cols1,
        placeholder := s"Paste/write molecule or create from schema tree...",
        tabindex := 0,
        moleculeStr,
      )
    )
  )

  def _codeSnippet(header: String,
                   renderTpe: String,
                   codeFragments: Frag*): TypedTag[Element] = _card(
    _cardHeader(h5(header)),
    _cardBody(
      pre(
        marginBottom := 0,
        code(
          cls := renderTpe,
          codeFragments
        )
      )
    )
  )

  def _favoritesSnippet(favorites: Seq[Favorite],
                        curMolecule: String,
                        useFav: Favorite => () => Unit,
                        retractFav: String => () => Unit
                       ): TypedTag[Element] = _card(
    _cardHeader(h5("Favorites")),
    _cardBody(
      _favoritesTable(favorites, curMolecule, useFav, retractFav)
    )
  )


  def _txSnippet(msg: String): TypedTag[Element] = _card(
    _cardHeader(h5("Transaction")),
    _cardBody(
      table(
        cls := "tableTx",
        id := "txSnippetTable",
        tr(td(msg))
      )
    )
  )

  def _entitySnippet(msg: String): TypedTag[Element] = _card(
    _cardHeader(h5("Entity")),
    _cardBody(
      table(
        cls := "tableEntity",
        id := "entitySnippetTable",
        tr(td(msg))
      )
    )
  )

  def _entityHistorySnippet(msg: String): TypedTag[Element] = _card(
    _cardHeader(
      h5("Entity History", display.`inline-block`),
      span(id := "entityHistoryEid", paddingLeft := 30, float.right)
    ),
    _cardBody(
      table(
        cls := "tableTx",
        id := "entityHistorySnippetTable",
        tr(td(msg))
      )
    )
  )

  def _cacheSnippet(molecules: Seq[String],
                    curMolecule: String,
                    curFavMolecules: Seq[String],
                    resetCache: () => Unit,
                    useCached: String => () => Unit,
                    addFav: String => () => Unit,
                    removeCached: String => () => Unit
                   ): TypedTag[Element] = _card(
    _cardHeader(
      h5("Cache",  display.`inline-block`),
      span(
        paddingLeft := 30,
        float.right,
        a(cls := "discrete", href := "#", "reset", onclick := resetCache))
    ),
    _cardBody(
      _cacheList(molecules, curMolecule, useCached, curFavMolecules, addFav, removeCached)
    )
  )


  val hljs = script(
    """$(document).ready(function() {
      |  $('pre code').each(function(i, block) {
      |    hljs.highlightBlock(block);
      |  });
      |});""".stripMargin)

  def _snippets(snippets: Frag*): TypedTag[Div] = div(
    paddingTop := 15,
    paddingLeft := 20,
    snippets,
    hljs
  )
}
