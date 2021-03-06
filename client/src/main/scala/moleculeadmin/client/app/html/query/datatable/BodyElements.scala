package moleculeadmin.client.app.html.query.datatable

import molecule.util.DateHandling
import moleculeadmin.client.app.css.Color
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.app.logic.query.QueryState.{curUrl, _}
import moleculeadmin.client.app.logic.query.keyEvents.MarkerToggling
import org.scalajs.dom.html.{Element, Span, TableCell, TableRow}
import org.scalajs.dom.raw.Node
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import util.client.rx.RxBindings


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
      if (togglers(0)) toggleStar()
      if (togglers(1)) toggleFlag()
      if (togglers(2)) toggleCheck()
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

  def _tdOneStr(cellId: String, prio: String = ""): TypedTag[TableCell] = td(
    id := cellId,
    cls := s"str$prio",
    cursor.default
  )

  def _tdOneNum(cellId: String, prio: String = ""): TypedTag[TableCell] = td(
    id := cellId,
    cls := s"num$prio",
    cursor.default
  )

  def _tdOneAggr(cellId: String, prio: String = ""): TypedTag[TableCell] = td(
    id := cellId,
    cls := s"num$prio",
    noAggrEdit
  )

  def _tdOneDate(cellId: String, prio: String = ""): TypedTag[TableCell] = td(
    id := cellId,
    cls := s"date$prio"
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
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    td(
      id := cellId,
      cls := Rx((if (eid == curEntity()) "eidChosen" else "eid") + prio),
      onmouseover := setCurEid,
      onclick := lockCurEid,
      // todo: skip span and do css magic to avoid squeezing overflow
      span(
        _xRetract(retract),
        eid,
        i(cls := starCls, onclick := starToggle),
        i(cls := flagCls, onclick := flagToggle),
        i(cls := checkCls, onclick := checkToggle),
      ),
    )
  }
  def _tdOneRef(
    cellId: String,
    ref: Long,
    curEntity: rx.Var[Long],
    setCurEid: Long => () => Unit,
    lockCurEid: Long => () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (ref == curEntity()) "eidChosen" else "eid") + prio),
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
    markRow: String => () => Unit
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
    markRow: String => () => Unit
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
    markRow: String => () => Unit
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
    update: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val refStr = optValue.fold("")(_.toString)
    val ref    = optValue.fold(0L)(_.toLong)
    td(
      id := cellId,
      cls := Rx((if (ref > 0L && ref == curEntity()) "eidChosen" else "eid") + prio),
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
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val ref = optValue.fold("")(_.toString)
    td(
      id := cellId,
      cls := s"num$prio",
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
    markRow: String => () => Unit
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
    showAll: Boolean,
    prio: String = ""
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := s"str$prio",
    attr("card") := 2,
    if (showAll)
      ul(vs.sorted.map(v => li(truncateDateStr(v))))
    else
      expandingList(vs.sorted.map(v => li(truncateDateStr(v))))
  )

  def _tdManyDouble(
    cellId: String,
    vs: List[Double],
    showAll: Boolean,
    prio: String = ""
  ): TypedTag[TableCell] = td(
    id := cellId,
    cls := s"num$prio",
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
    max: Int = defaultSize,
    prio: String = ""
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
      val moreText     = s"${vs.size - max} more..."
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
      cls := prio,
      attr("card") := 2,
      list
    )
  }


  def _tdManyStringItemEdit(
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

  def _tdManyStringEdit(
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
    ul(vs.map(li(_)))
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
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    td(
      id := cellId,
      cls := prio,
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

  def _tdMapItems(cellId: String, prio: String = ""): TypedTag[TableCell] = td(
    id := cellId,
    cls := s"mapPairs$prio",
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
    vs: Map[String, String],
    prio: String = ""
  ): TypedTag[TableCell] =
    mapCell(
      cellId,
      vs.toSeq.sortBy(_._1),
      (v: String) => td(_str2frags(v)),
      prio = prio
    )

  def _tdMapDate(
    cellId: String,
    vs: Map[String, String],
    prio: String = ""
  ): TypedTag[TableCell] =
    mapCell(
      cellId,
      vs.toSeq.sortBy(_._1),
      (v: String) => td(truncateDateStr(v)),
      prio = prio
    )

  def _tdMapStrOther(
    cellId: String,
    vs: Map[String, String],
    prio: String = ""
  ): TypedTag[TableCell] =
    mapCell(
      cellId,
      vs.toSeq.sortBy(_._1),
      (v: String) => td(v),
      prio = prio
    )

  def _tdMapDouble(
    cellId: String,
    vs: Map[String, Double],
    prio: String = ""
  ): TypedTag[TableCell] =
    mapCell(
      cellId,
      vs.toSeq.sortBy(_._1),
      (v: Double) => td(v),
      prio = prio
    )


  // Hard-coding each combination to make row rendering as fast as possible

  lazy val noAggrEdit = onclick := { () =>
    window.alert(
      "Entity id, aggregates and transaction values can't be edited."
    )
  }

  // t ----------------------------------------

  def _tdOneT(
    cellId: String,
    t: Long,
    curT: rx.Var[Long],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curT() == t) "txChosen" else "tx") + prio),
    t, onmouseover := mouseover, noAggrEdit
  )


  def _tdOneT_tx(
    cellId: String,
    t: Long,
    tx: Long,
    curT: rx.Var[Long],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curT() == t) "txChosen" else "tx") + prio),
    t, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneT_inst(
    cellId: String,
    t: Long,
    txInstant: String,
    curT: rx.Var[Long],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curT() == t) "txChosen" else "tx") + prio),
    t, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneT_tx_inst(
    cellId: String,
    t: Long,
    tx: Long,
    txInstant: String,
    curT: rx.Var[Long],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curT() == t) "txChosen" else "tx") + prio),
    t, onmouseover := mouseover, noAggrEdit
  )

  // tx ----------------------------------------

  def _tdOneTx(
    cellId: String,
    tx: Long,
    curTx: rx.Var[Long],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curTx() == tx) "txChosen" else "tx") + prio),
    tx, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTx_t(
    cellId: String,
    tx: Long,
    t: Long,
    curTx: rx.Var[Long],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curTx() == tx) "txChosen" else "tx") + prio),
    tx, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTx_inst(
    cellId: String,
    tx: Long,
    txInstant: String,
    curTx: rx.Var[Long],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curTx() == tx) "txChosen" else "tx") + prio),
    tx, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTx_t_inst(
    cellId: String,
    tx: Long,
    t: Long,
    txInstant: String,
    curTx: rx.Var[Long],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curTx() == tx) "txChosen" else "tx") + prio),
    tx, onmouseover := mouseover, noAggrEdit
  )

  // txInstant -----------------------------------

  def _tdOneTxInstant(
    cellId: String,
    txInstant: String,
    curTxInstant: rx.Var[String],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curTxInstant() == txInstant) "txChosen" else "tx") + prio),
    txInstant, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTxInstant_t(
    cellId: String,
    txInstant: String,
    t: Long,
    curTxInstant: rx.Var[String],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curTxInstant() == txInstant) "txChosen" else "tx") + prio),
    txInstant, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTxInstant_tx(
    cellId: String,
    txInstant: String,
    tx: Long,
    curTxInstant: rx.Var[String],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curTxInstant() == txInstant) "txChosen" else "tx") + prio),
    txInstant, onmouseover := mouseover, noAggrEdit
  )
  def _tdOneTxInstant_t_tx(
    cellId: String,
    txInstant: String,
    t: Long,
    tx: Long,
    curTxInstant: rx.Var[String],
    mouseover: () => Unit,
    prio: String = ""
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    id := cellId,
    cls := Rx((if (curTxInstant() == txInstant) "txChosen" else "tx") + prio),
    txInstant, onmouseover := mouseover, noAggrEdit
  )

}
