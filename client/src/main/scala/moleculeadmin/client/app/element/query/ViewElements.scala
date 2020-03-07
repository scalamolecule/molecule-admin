package moleculeadmin.client.app.element.query

import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Div, Element}
import org.scalajs.dom.window
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

trait ViewElements extends SubMenuElements {

  def _moleculeView(
    rows1: Int,
    cols1: Int,
    moleculeStr: String,
    addQuery: () => Unit,
    isSavedQuery: Boolean
  ): TypedTag[Element] =
    _card(
      _cardHeader(
        height := 23,
        h5("Molecule", display.`inline-block`),
        if (isSavedQuery) {
          span(
            paddingLeft := 30, float.right, "fav", color := Color.textGray,
            onclick := { () =>
              window.alert(s"`$moleculeStr` is already a saved query.")
            })
        } else {
          span(
            paddingLeft := 30, float.right,
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
  ): TypedTag[Element] =
    _card(
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

  def _txView(msg: String): TypedTag[Element] =
    _card(
      _cardHeader(h5("Transaction")),
      _cardBody(
        table(
          cls := "tableTx",
          id := "txViewTable",
          tr(td(msg))
        )
      )
    )

  def _entityView(msg: String): TypedTag[Element] =
    _card(
      _cardHeader(h5("Entity")),
      _cardBody(
        table(
//          cls := "tableEntityX",
//          id := "entityViewTableBackRef",
          width := "100%",
          color := Color.link,
//          cursor.pointer,
//          tr(
//            onclick := { () => window.alert("yeah") },
//            td(":Ns.int"),
//            td(
//              fontFamily := "Courier",
//              textAlign.right,
//              "208371"
//            ),
//          ),
          tr(
            //            cursor.pointer,
            td(":Ns.ref1"),
            td(
//            onclick := { () => window.alert("yeah") },
              fontFamily := "Courier",
              textAlign.right,
              a(href:="#", "5288")
            ),
          ),
          tr(td("hej")),
          tr(td("don")),
        ),
        hr(margin := "4px -3px"),
        table(
          cls := "tableEntity",
          id := "entityViewTable",
          tr(td(msg))
        )
      ),
      //      _cardBody(
      //        table(
      //          cls := "tableEntity",
      //          id := "entityViewTable2",
      //          tr(td("hejhej"))
      //        )
      //      )
    )

  def _entityHistoryView(msg: String): TypedTag[Element] =
    _card(
      _cardHeader(
        height := 23,
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

  def _urlView(url: String): TypedTag[Element] =
    _card(
      _cardHeader(
        h5("Url"),
        h6(url)
      ),
      _cardBody(
        iframe(
          id := "urlView",
          src := url,
          height := "800px",
          width := "1000px",
          border := 0
        )
      )
    )
}
