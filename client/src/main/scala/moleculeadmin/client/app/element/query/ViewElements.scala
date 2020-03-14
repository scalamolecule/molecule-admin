package moleculeadmin.client.app.element.query

import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Element, HR, Span, Table}
import org.scalajs.dom.{document, window}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{height, _}
import org.scalajs.dom.raw.{Element => RawElement}


trait ViewElements extends SubMenuElements with RxBindings {

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


  // Transaction view -----------------------

  def _txView(msg: Frag): TypedTag[Element] =
    _card(
      _cardHeader(
        height := 23,
        h5("Transaction", display.`inline-block`),
        span(id := "txViewEid", paddingLeft := 30, float.right)
      ),
      _cardBody(
        table(
          cls := "tableTx",
          id := "txViewTable",
          tr(td(msg))
        )
      )
    )

  def _spinTxView(view: RawElement, msg: String): Unit = {
    view.innerHTML = ""
    view.appendChild(
      tr(td(span(msg, _sync(10)))).render
    )
  }


  // Entity view -----------------------

  def _entityView(
    showBackRefsLink: Frag,
    backRefsSpan: Frag,
    entityData: Frag
  ): TypedTag[Element] =
    _card(
      minWidth := 200,
      _cardHeader(
        height := 23,
        h5("Entity", display.`inline-block`),
        showBackRefsLink
      ),
      _cardBody(
        backRefsSpan,
        entityData
      )
    )

  def _entityShowBackRefs(
    toggleBackRefs: () => Unit,
    show: String
  ) =
    a(
      href := "#",
      paddingLeft := 30,
      float.right,
      onclick := toggleBackRefs,
      show + " back-refs"
    )

  def _backRefLink(attrFull: String, count: Int, url: String) =
    a(
      href := url,
      target := "_blank",
      rel := "noopener noreferrer",
      div(
        attrFull,
        span(count)
      )
    ).render

  def _backRefSeparator: HR = hr(margin := "4px -3px").render

  def _entityBackRefs: Span =
    span(
      cls := "tableEntityBackRefs",
    ).render

  def _entityDatoms(msg: String): TypedTag[Table] =
    table(
      cls := "tableEntity",
      id := "entityViewTable",
      tr(td(msg))
    )


  // Entity History view -----------------------

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


  // Live URL view -----------------------

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
