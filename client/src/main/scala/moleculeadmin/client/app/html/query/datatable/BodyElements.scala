package moleculeadmin.client.app.html.query.datatable

import util.client.rx.RxBindings
import molecule.util.DateHandling
import moleculeadmin.client.app.logic.query.QueryState.curUrl
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Anchor, Element, Span, TableCell, TableRow}
import org.scalajs.dom.raw.Node
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import org.scalajs.dom.{document, window}
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.keyEvents.MarkerToggling


trait BodyElements
  extends AppElements
    with DateHandling
    with RxBindings
    with MarkerToggling {


  def _mkRow(
    rowIndex: Int,
    cells: JsDom.TypedTag[TableCell]*
  ): TableRow = tr(
    cls := "view",
    td(rowIndex + 1),
    onmouseover := { () =>
      if (toggling) {
        toggler match {
          case "s" => toggleStar()
          case "f" => toggleFlag()
          case "c" => toggleCheck()
          case _   =>
        }
      }

    },
    cells
  ).render

  def _xRetract(retract: () => Unit): TypedTag[Element] = i(
    cls := "oi oi-x retractEntity",
    onclick := retract
  )

  // Use span to ease editing conversions between txt/html
  def _urlSpan(url: String, edit: Boolean)(implicit ctx: Ctx.Owner): TypedTag[Span] = {
    if (edit)
      span(
        color := Color.link,
        cursor.text,
        onmouseover := { () => curUrl() = url },
        url
      )
    else
      span(
        color := Color.link,
        cursor.pointer,
        onclick := { () => window.open(url) },
        onmouseover := { () => curUrl() = url },
        url
      )
  }

  // Card one ======================================================

  def _tdOneStr(cellId: String): TypedTag[TableCell] = td(
    id := cellId,
    cls := "str",
    cursor.default
  )

  def _tdOneNum(cellId: String): TypedTag[TableCell] = td(
    id := cellId,
    cls := "num",
    cursor.default
  )

  def _tdOneAggr(cellId: String): TypedTag[TableCell] = td(
    id := cellId,
    cls := "num",
    noAggrEdit
  )

  def _tdOneDate(
    cellId: String
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := "date"
  )

  def _tdOneEid(
    cellId: String,
    eid: Long,
    curEntity: rx.Var[Long],
    setCurEid: () => Unit,
    lockCurEid: () => Unit,
    starCls: String,
    flagCls: String,
    checkCls: String,
    retract: () => Unit,
    starToggle: () => Unit,
    flagToggle: () => Unit,
    checkToggle: () => Unit,
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    td(
      id := cellId,
      cls := Rx(if (eid == curEntity()) "eidChosen" else "eid"),
      _xRetract(retract),
      eid,
      i(cls := starCls, onclick := starToggle),
      i(cls := flagCls, onclick := flagToggle),
      i(cls := checkCls, onclick := checkToggle),
      onmouseover := setCurEid,
      onclick := lockCurEid
    )
  }
  def _tdOneRef(
    cellId: String,
    ref: Long,
    curEntity: rx.Var[Long],
    setCurEid: Long => () => Unit,
    lockCurEid: Long => () => Unit,
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
    attr("card") := 1,
    ref,
    onmouseover := setCurEid(ref),
    onclick := lockCurEid(ref)
  )

  def _tdOneStrEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    strFrag: Frag,
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = {
    td(
      id := cellId,
      cls := cellClass,
      attr("card") := 1,
      attr("eid") := eid,
      contenteditable := true,
      onclick := markRow(cellId),
      onblur := update,
      strFrag,
    )
  }

  def _tdOneDateEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    optValue: Option[String],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellClass,
    attr("card") := 1,
    attr("eid") := eid,
    contenteditable := true,
    onclick := markRow(cellId),
    onblur := update,
    optValue.fold("")(truncateDateStr)
  )

  // T String/Double
  def _tdOneNumEdit[T](
    cellId: String,
    cellClass: String,
    eid: Long,
    optValue: Option[T],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellClass,
    attr("card") := 1,
    attr("eid") := eid,
    contenteditable := true,
    onclick := markRow(cellId),
    onblur := update,
    optValue.fold("")(_.toString)
  )

  def _tdOneRefEdit(
    cellId: String,
    eid: Long,
    optValue: Option[Double],
    curEntity: rx.Var[Long],
    setCurEid: Long => () => Unit,
    lockCurEid: Long => () => Unit,
    update: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val refStr = optValue.fold("")(_.toString)
    val ref    = optValue.fold(0L)(_.toLong)
    td(
      id := cellId,
      cls := Rx(if (ref > 0L && ref == curEntity()) "eidChosen" else "eid"),
      refStr,
      onmouseover := setCurEid(ref),
      onclick := lockCurEid(ref),
      attr("card") := 1,
      attr("eid") := eid,
      contenteditable := true,
      onblur := update,
    )
  }

  def _tdOneRefEdit2(
    cellId: String,
    eid: Long,
    optValue: Option[Double],
    update: () => Unit,
    markRow: String => () => Unit,
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val ref = optValue.fold("")(_.toString)
    td(
      id := cellId,
      cls := "num",
      ref,
      attr("card") := 1,
      attr("eid") := eid,
      contenteditable := true,
      onclick := markRow(cellId),
      onblur := update,
    )
  }

  def _tdOneEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    optValue: Option[String],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellClass,
    attr("card") := 1,
    attr("eid") := eid,
    contenteditable := true,
    onclick := markRow(cellId),
    onblur := update,
    optValue.fold("")(_.toString)
  )


  // Card many ========================================================

  def _tdMany(cellId: String, clazz: String): TypedTag[TableCell] = td(
    id := cellId,
    cls := clazz,
    attr("card") := 2,
    cursor.default
  )

  def _tdManyStringUrl(
    cellId: String,
    strFrags: List[Seq[Frag]],
    cellType: String,
    showAll: Boolean
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellType,
    attr("card") := 2,
    if (showAll)
      ul(strFrags.map(li(_)))
    else
      expandingList(strFrags.map(li(_)), true)
  )

  def _tdManyString(
    cellId: String,
    vs: List[String],
    cellType: String,
    showAll: Boolean
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellType,
    attr("card") := 2,
    if (showAll)
      ul(vs.sorted.map(v => li(_str2frags(v))))
    else
      expandingList(
        vs.sorted.map(v =>
          li(_str2frags(v))
        ),
        true
      )
  )

  def _tdManyDate(
    cellId: String,
    vs: List[String],
    showAll: Boolean
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := "str",
    attr("card") := 2,
    if (showAll)
      ul(vs.sorted.map(v => li(truncateDateStr(v))))
    else
      expandingList(vs.sorted.map(v => li(truncateDateStr(v))))
  )

  def _tdManyDouble(
    cellId: String,
    vs: List[Double],
    showAll: Boolean
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := "num",
    attr("card") := 2,
    if (showAll)
      ul(vs.sorted.map(li(_)))
    else
      expandingList(vs.sorted.map(li(_)))
  )

  def _tdManyRef(
    cellId: String,
    vs: List[Double],
    curEntity: rx.Var[Long],
    setCurEid: Long => () => Unit,
    lockCurEid: Long => () => Unit,
    showAll: Boolean = false,
    max: Int = defaultSize
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val vsSorted = vs.sorted
    val list     = ul().render
    def addToList(ref: Long): Node = list.appendChild(
      li(
        cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
        ref,
        onmouseover := setCurEid(ref),
        onclick := lockCurEid(ref)
      ).render
    )
    if (showAll || vs.size <= max) {
      vsSorted.foreach(v => addToList(v.toLong))
    } else {
      val moreText     = (vs.size - max) + " more..."
      val collapseText = "Collapse..."
      val toggleLink   = a(href := "#", moreText).render
      val toggleItem   = li(toggleLink).render
      toggleLink.onclick = _ => {
        val collapsed = list.lastChild.textContent != collapseText
        list.innerHTML = ""
        (if (collapsed) vsSorted else vsSorted.take(max))
          .foreach(v => addToList(v.toLong))
        toggleLink.innerHTML = if (collapsed) collapseText else moreText
        list.appendChild(toggleItem)
      }
      vsSorted.take(max).foreach(v => addToList(v.toLong))
      list.appendChild(toggleItem)
    }
    td(
      id := cellId,
      attr("card") := 2,
      list
    )
  }


  def _tdManyStringEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    strFrags: List[Seq[Frag]],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellClass,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onclick := markRow(cellId),
    onblur := update,
    ul(strFrags.map(li(_)))
  )

  def _tdManyDateEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    vs: List[String],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellClass,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onclick := markRow(cellId),
    onblur := update,
    ul(vs.map(li(_)))
  )

  def _tdManyStringOtherEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    vs: List[String],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellClass,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onclick := markRow(cellId),
    onblur := update,
    ul(vs.map(li(_)))
  )

  def _tdManyStringBigEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    vs: List[String],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellClass,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onclick := markRow(cellId),
    onblur := update,
    ul(vs.map(li(_)))
  )

  def _tdManyDoubleEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    vs: List[Double],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellClass,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onclick := markRow(cellId),
    onblur := update,
    vs.flatMap(v => Seq(v: Frag, br))
  )

  def _tdManyRefEdit(
    cellId: String,
    eid: Long,
    curEntity: rx.Var[Long],
    refs: List[Long],
    setCurEid: Long => () => Unit,
    lockCurEid: Long => () => Unit,
    update: () => Unit,
    markRow: String => () => Unit,
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    td(
      id := cellId,
      attr("card") := 2,
      attr("eid") := eid,
      contenteditable := true,
      onclick := markRow(cellId),
      onblur := update,
      ul(
        refs.map(ref =>
          li(
            cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
            ref,
            onmouseover := setCurEid(ref),
            onclick := lockCurEid(ref)
          )
        )
      )
    )
  }

  def _tdManyRefGroupEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    refs: List[Long],
    setCurEid: Long => () => Unit,
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] = {
    td(
      id := cellId,
      cls := cellClass,
      attr("card") := 2,
      attr("eid") := eid,
      contenteditable := true,
      onclick := markRow(cellId),
      onblur := update,
      ul(
        refs.map(ref =>
          li(
            cls := "num",
            ref,
            onmouseover := setCurEid(ref)
          )
        )
      )
    )
  }


  // Map ==============================================================

  def _tdMapItems(cellId: String): TypedTag[TableCell] = td(
    id := cellId,
    cls := "mapPairs",
    cursor.default
  )

  def _tdMapEdit[T](
    cellId: String,
    cellClass: String,
    eid: Long,
    update: () => Unit,
    markRow: String => () => Unit,
    pairs: Seq[(String, T)],
    processPair: (String, T) => Frag,
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := cellClass,
    attr("card") := 3,
    attr("eid") := eid,
    contenteditable := true,
    onclick := markRow(cellId),
    onblur := update,
    ul(pairs.map { case (k, v) => li(processPair(k, v)) })
  )

  def _tdMapStrEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    vs: Map[String, String],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] =
    _tdMapEdit(cellId, cellClass, eid, update, markRow,
      vs.toSeq.sortBy(_._1),
      (k: String, v: String) => _str2frags(k + " -> " + v)
    )

  def _tdMapDateEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    vs: Map[String, String],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] =
    _tdMapEdit(cellId, cellClass, eid, update, markRow,
      vs.toSeq.sortBy(_._1),
      (k: String, v: String) => k + " -> " + truncateDateStr(v)
    )

  def _tdMapStrOtherEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    vs: Map[String, String],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] =
    _tdMapEdit(cellId, cellClass, eid, update, markRow,
      vs.toSeq.sortBy(_._1),
      (k: String, v: String) => k + " -> " + v
    )

  def _tdMapDoubleEdit(
    cellId: String,
    cellClass: String,
    eid: Long,
    vs: Map[String, Double],
    update: () => Unit,
    markRow: String => () => Unit,
  ): TypedTag[TableCell] =
    _tdMapEdit(cellId, cellClass, eid, update, markRow,
      vs.toSeq.sortBy(_._1),
      (k: String, v: Double) => k + " -> " + v
    )


  def _tdMapStr(
    cellId: String,
    vs: Map[String, String]
  ): TypedTag[TableCell] =
    mapCell(
      cellId,
      vs.toSeq.sortBy(_._1), (v: String) => td(_str2frags(v))
    )

  def _tdMapDate(
    cellId: String,
    vs: Map[String, String]
  ): TypedTag[TableCell] =
    mapCell(
      cellId,
      vs.toSeq.sortBy(_._1), (v: String) => td(truncateDateStr(v))
    )

  def _tdMapStrOther(
    cellId: String,
    vs: Map[String, String]
  ): TypedTag[TableCell] =
    mapCell(
      cellId,
      vs.toSeq.sortBy(_._1), (v: String) => td(v)
    )

  def _tdMapDouble(
    cellId: String,
    vs: Map[String, Double]
  ): TypedTag[TableCell] =
    mapCell(
      cellId,
      vs.toSeq.sortBy(_._1), (v: Double) => td(v)
    )


  // Hard-coding each combination to make row rendering as fast as possible

  // t ----------------------------------------

  def _tdOneT(
    cellId: String,
    t: Long,
    curT: rx.Var[Long],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curT() == t) "txChosen" else "tx"),
    t, onmouseover := mouseover, noAggrEdit
  )


  def _tdOneT_tx(
    cellId: String,
    t: Long,
    tx: Long,
    curT: rx.Var[Long],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curT() == t) "txChosen" else "tx"),
    t, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneT_inst(
    cellId: String,
    t: Long,
    txInstant: String,
    curT: rx.Var[Long],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curT() == t) "txChosen" else "tx"),
    t, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneT_tx_inst(
    cellId: String,
    t: Long,
    tx: Long,
    txInstant: String,
    curT: rx.Var[Long],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curT() == t) "txChosen" else "tx"),
    t, onmouseover := mouseover, noAggrEdit
  )

  // tx ----------------------------------------

  def _tdOneTx(
    cellId: String,
    tx: Long,
    curTx: rx.Var[Long],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curTx() == tx) "txChosen" else "tx"),
    tx, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTx_t(
    cellId: String,
    tx: Long,
    t: Long,
    curTx: rx.Var[Long],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curTx() == tx) "txChosen" else "tx"),
    tx, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTx_inst(
    cellId: String,
    tx: Long,
    txInstant: String,
    curTx: rx.Var[Long],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curTx() == tx) "txChosen" else "tx"),
    tx, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTx_t_inst(
    cellId: String,
    tx: Long,
    t: Long,
    txInstant: String,
    curTx: rx.Var[Long],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curTx() == tx) "txChosen" else "tx"),
    tx, onmouseover := mouseover, noAggrEdit
  )

  // txInstant -----------------------------------

  def _tdOneTxInstant(
    cellId: String,
    txInstant: String,
    curTxInstant: rx.Var[String],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curTxInstant() == txInstant) "txChosen" else "tx"),
    txInstant, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTxInstant_t(
    cellId: String,
    txInstant: String,
    t: Long,
    curTxInstant: rx.Var[String],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curTxInstant() == txInstant) "txChosen" else "tx"),
    txInstant, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTxInstant_tx(
    cellId: String,
    txInstant: String,
    tx: Long,
    curTxInstant: rx.Var[String],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curTxInstant() == txInstant) "txChosen" else "tx"),
    txInstant, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTxInstant_t_tx(
    cellId: String,
    txInstant: String,
    t: Long,
    tx: Long,
    curTxInstant: rx.Var[String],
    mouseover: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx(if (curTxInstant() == txInstant) "txChosen" else "tx"),
    txInstant, onmouseover := mouseover, noAggrEdit
  )

}
