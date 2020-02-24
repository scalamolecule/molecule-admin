package moleculeadmin.client.app.element

import moleculeadmin.client.jsdom.Bootstrap
import moleculeadmin.client.jsdom.Bootstrap
import org.scalajs.dom.{document, window}
import org.scalajs.dom.html.{Div, Element, LI, Span, TableCell, TableRow, UList}
import scalatags.JsDom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


trait AppElements extends Bootstrap {

  def _containerFluid: JsDom.TypedTag[Div] = div(cls := "container-fluid")
  def _containerFluid2: JsDom.TypedTag[Div] = _containerFluid(paddingLeft := 0)

  def _row: JsDom.TypedTag[Div] = div(cls := "row")

  def _rowCol: JsDom.TypedTag[Div] = div(cls := "col")
  def _rowCol1: JsDom.TypedTag[Div] = _rowCol(paddingTop := 10)
  def _rowCol2: JsDom.TypedTag[Div] = _rowCol(paddingRight := 0)

  def _rowColAuto: JsDom.TypedTag[Div] = div(cls := "col-md-auto")
  def _rowColAuto2: JsDom.TypedTag[Div] = _rowColAuto(paddingRight := 0, paddingTop := "10px", verticalAlign.middle)
  def _rowColAuto3: JsDom.TypedTag[Div] = _rowColAuto(paddingLeft := 0)
  def _rowColAuto4: JsDom.TypedTag[Div] = _rowColAuto(paddingLeft := 0, paddingRight := 0, paddingTop := "10px")
  def _rowColAuto5: JsDom.TypedTag[Div] = _rowColAuto(paddingLeft := 0, paddingBottom := 10)
  def _rowColAuto6: JsDom.TypedTag[Div] = _rowColAuto(marginLeft := 15, paddingTop := 10, paddingRight := 0)


  def _formatEmpty(s: String): String = if (s.trim.isEmpty) s"{$s}" else s

  def _optStr2frags(optStr: Option[String]): Seq[Frag] =
    optStr.fold(Seq.empty[Frag])(_str2frags)

  def _str2frags(s0: String): Seq[Frag] = {
    // All extra spaces as &nbsp;
    val s = s0.replaceAllLiterally("  ", "\u00a0 ") // &nbsp;
    if (s.contains('\n')) {
      if (s0.replaceAllLiterally("\n", " ").trim.isEmpty)
        s"{$s}".split("\n").toSeq.flatMap(s1 => Seq(StringFrag(s1), br)).init
      else
        s.split("\n").toSeq.flatMap(s1 => Seq(StringFrag(s1), br)).init
    } else if (s0.trim.isEmpty) {
      Seq(s"{$s}")
    } else {
      Seq(s)
    }
  }

  def _html2str(html: String): String = {
    val str = html
      .replaceFirst("<span[^>]*>", "")
      .replaceFirst("</span>", "")
      .replaceAllLiterally("<span></span>", "")
      .replaceAllLiterally("&nbsp;", " ")
      .replaceAllLiterally("&lt;", "<")
      .replaceAllLiterally("&gt;", ">")
      .replaceAllLiterally("<br>", "\n")
    if (str.startsWith("{"))
    // Invisible empty strings surrounded with { }
      if (str.endsWith("}") && str.tail.init.replaceAllLiterally("\n", " ").trim.isEmpty)
        str.tail.init
      else
        str
    else
      str
  }

  def _shortcut(underlined: String, tail: String, prefix: String = ""): TypedTag[Span] =
    span(
      prefix,
      span(underlined, textDecoration.underline),
      tail
    )

  def _cbLabel(key: String, txt: String): TypedTag[Span] =
    span(
      span(key, marginLeft := 5, marginRight := 10),
      txt
    )

  def _cb(id1: String, txt: Frag, checked1: Boolean, toggle: () => Unit): TypedTag[Div] =
    div(
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
      ),
      whiteSpace.nowrap
    )

  val hljs =
    script(
      """$(document).ready(function() {
        |  $('pre code').each(function(i, block) {
        |    hljs.highlightBlock(block);
        |  });
        |});""".stripMargin)

  def _cardsContainer(cards: Frag*): TypedTag[Div] =
    div(
      paddingTop := 15,
      paddingLeft := 20,
      cards,
      hljs
    )

  val defaultSize = 10

  protected def expandingList(
    items: Seq[TypedTag[LI]],
    showDisc: Boolean = false,
    max: Int = defaultSize
  ): UList = {
    val list = if (showDisc)
      ul(items.take(max)).render
    else
      ul(
        //        listStyleType.none,
        style := "list-style-type: none; padding-inline-start: 0;",
        items.take(max)
      ).render

    // Open/close large collections of items
    if (items.size > max) {
      list.appendChild(li(a(href := "#", (items.size - max) + " more...")).render)
      list.onclick = _ => {
        // collapsed when last li has link element
        val collapsed = list.lastChild.lastChild.nodeType == 1
        list.innerHTML = ""
        if (collapsed) {
          // expand all
          items.map(l => list.appendChild(l.render))
        } else {
          // collapse to max items and add expand link
          items.take(max).map(l => list.appendChild(l.render))
          list.appendChild(li(a(href := "#", (items.size - max) + " more...")).render)
        }
      }

    }
    list
  }

  object mark {
    val starOn   = "fas fa-star starOn"
    val starOff  = "far fa-star starOff"
    val flagOn   = "fas fa-flag flagOn"
    val flagOff  = "far fa-flag flagOff"
    val checkOn  = "fas fa-check-circle checkOn"
    val checkOff = "fas fa-check checkOff"
  }

  val noEdit = onclick := { () =>
//    window.alert(
//      "Namespace must have an entity id `e` column first to allow editing."
//    )
  }

  val noAggrEdit = onclick := { () =>
    window.alert(
      "Entity id, aggregates and transaction values can't be edited."
    )
  }

  def mapRow(k: String, vCell: TypedTag[TableCell]): TypedTag[TableRow] =
    tr(td(k), td("➜"), vCell)

  def mapCell[T](
    rawPairs: Seq[(String, T)],
    processValue: T => TypedTag[TableCell],
    asserted: Boolean = true
  ): TypedTag[TableCell] = {
    td(
      if (asserted) () else cls := "retracted",
      table(cls := "mapPairs",
        rawPairs.map {
          case (k, v) => mapRow(k, processValue(v))
        }
      )
    )
  }

  def _sync(leftMarg: Int = 0): JsDom.TypedTag[Element] =
    i(cls := "fa fa-sync fa-spin", color := "#ff8137",
      if (leftMarg > 0) marginLeft := leftMarg else ())

  def _btn2(labelStr: String, id1: String, marked: String, onclick1: () => Unit): JsDom.TypedTag[Div] = div(
    cls := "btn btn-light btn-sm no2" + marked,
    id := id1,
    onclick := onclick1,
    labelStr
  )


  def _card: TypedTag[Div] = div(cls := "card",
    marginBottom := 20,
    borderColor := "#c3c3c3",
    width := "fit-content",
  )
  def _cardHeader: JsDom.TypedTag[Div] = div(
    cls := "card-header",
    padding := "3px 6px 4px",
    backgroundColor := "#e8e8e8"
  )
  def _cardBody: JsDom.TypedTag[Div] = div(cls := "card-body", padding := 3)


  def _reloadMenuAim(): Unit = {
    val scriptWrapper = document.getElementById("scriptWrapper")
    val url           = "/assets/javascripts/jquery.menu-aim.options.js"
    scriptWrapper.innerHTML = ""
    scriptWrapper.appendChild(
      script(
        s"""
           |$$.getScript({
           |   url: "$url",
           |   cache: true
           |});
        """.stripMargin
      ).render
    )
    ()
  }

  def _enablePopovers(): Unit = {
    val scriptWrapper = document.getElementById("scriptWrapper")
    scriptWrapper.innerHTML = ""
    scriptWrapper.appendChild(
      script(
        """
          |$(function () {
          |  $('[data-toggle="popover"]').popover()
          |})
        """.stripMargin
        //        """
        //          |$(function () {
        //          |  $('.help-popover').popover({
        //          |    container: 'body'
        //          |  })
        //          |})
        //        """.stripMargin
      ).render
    )
    ()
  }

  def _enableTooltips(): Unit = {
    val scriptWrapper = document.getElementById("tooltipsScriptWrapper")
    scriptWrapper.innerHTML = ""
    scriptWrapper.appendChild(
      script(
        raw(
          """
            |$(function () {
            |   window.alert('hej');
            |  $('[data-toggle="tooltip"]').tooltip()
            |})
        """.stripMargin
        )
      ).render
    )
    ()
  }

  def _rightSpace(s: Frag, rightPxs: Int = 15): JsDom.TypedTag[Span] = span(
    paddingRight := rightPxs,
    s
  )

}
