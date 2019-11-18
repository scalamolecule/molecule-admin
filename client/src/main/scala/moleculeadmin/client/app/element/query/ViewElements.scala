package moleculeadmin.client.app.element.query
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Div, Element, LI, Span, TableRow}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{h5, _}
import org.scalajs.dom.window
import moleculeadmin.shared.ast.query.SavedQuery

trait ViewElements extends SubMenuElements {

  val _avoidAutofocus = input(tpe := "hidden", autofocus := true)

  def _moleculeView(
    rows1: Int,
    cols1: Int,
    moleculeStr: String,
    addQuery: () => Unit,
    isSavedQuery: Boolean
  ): TypedTag[Element] = _card(
    _cardHeader(
      h5("Molecule", display.`inline-block`),
      if (isSavedQuery) {
        span(paddingLeft := 30, float.right, "fav", color := Color.textGray,
          onclick := { () =>
            window.alert(s"`$moleculeStr` is already a saved query.")
          })
      } else {
        span(paddingLeft := 30, float.right,
          a(cls := "discrete", href := "#", "fav", onclick := addQuery))
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

  def _codeView(
    header: String,
    renderTpe: String,
    codeFragments: Frag*
  ): TypedTag[Element] = _card(
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

  def _queriesView(
    savedQueries: Seq[SavedQuery],
    curMolecule: String,
    useQuery: SavedQuery => () => Unit,
    retractQuery: String => () => Unit
  ): TypedTag[Element] = _card(
    _cardHeader(h5("queries")),
    _cardBody(
      _queriesTable(savedQueries, curMolecule, useQuery, retractQuery)
    )
  )


  def _txView(msg: String): TypedTag[Element] = _card(
    _cardHeader(h5("Transaction")),
    _cardBody(
      table(
        cls := "tableTx",
        id := "txViewTable",
        tr(td(msg))
      )
    )
  )

  def _entityView(msg: String): TypedTag[Element] = _card(
    _cardHeader(h5("Entity")),
    _cardBody(
      table(
        cls := "tableEntity",
        id := "entityViewTable",
        tr(td(msg))
      )
    )
  )

  def _entityHistoryView(msg: String): TypedTag[Element] = _card(
    _cardHeader(
      h5("Entity History", display.`inline-block`),
      span(id := "entityHistoryEid", paddingLeft := 30, float.right)
    ),
    _cardBody(
      table(
        cls := "tableTx",
        id := "entityHistoryViewTable",
        tr(td(msg))
      )
    )
  )

  def _recentMoleculesView(
    recentMolecules: Seq[String],
    curMolecule: String,
    savedMolecules: Seq[String],
    resetRecentMolecules: () => Unit,
    useRecentMolecule: String => () => Unit,
    saveQuery: String => () => Unit,
    removeRecentMolecules: String => () => Unit
  ): TypedTag[Element] = _card(
    _cardHeader(
      h5("Cache", display.`inline-block`),
      span(
        paddingLeft := 30,
        float.right,
        a(cls := "discrete", href := "#", "reset", onclick := resetRecentMolecules))
    ),
    _cardBody(
      _recentMoleculesList(
        recentMolecules,
        curMolecule,
        useRecentMolecule,
        savedMolecules,
        saveQuery,
        removeRecentMolecules,
      )
    )
  )


  val hljs = script(
    """$(document).ready(function() {
      |  $('pre code').each(function(i, block) {
      |    hljs.highlightBlock(block);
      |  });
      |});""".stripMargin)

  def _views(views: Frag*): TypedTag[Div] = div(
    paddingTop := 15,
    paddingLeft := 20,
    views,
    hljs
  )
}
