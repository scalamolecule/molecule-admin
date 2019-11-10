package moleculeadmin.client.app.element.query.datatable
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.rxstuff.RxBindings
import molecule.util.DateHandling
import org.scalajs.dom.html.{TableCell, TableRow}
import org.scalajs.dom.raw.Node
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{ul, _}


trait BodyElements extends AppElements with DateHandling with RxBindings {


  def _mkRow(rowIndex: Int,
             cells: JsDom.TypedTag[TableCell]*): TableRow = tr(
    td(rowIndex + 1),
    cells
  ).render


  // Card one ======================================================

  def _tdNoEdit: TypedTag[TableCell] = td(noEdit)
  def _tdNoAggrEdit: TypedTag[TableCell] = td(noAggrEdit)
  def _tdOneNumNoEdit: TypedTag[TableCell] = td(cls := "num", noEdit)
  def _tdOneNumNoAggrEdit: TypedTag[TableCell] = td(cls := "num", noAggrEdit)
  def _tdOneDate: TypedTag[TableCell] = td(cls := "date", noEdit)
  def _tdOneEid(eid: Long,
                curEntity: rx.Var[Long],
                mouseover: Long => () => Unit
               )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (eid == curEntity()) "eidChosen" else "eid"),
    eid,
    onmouseover := mouseover(eid),
    noAggrEdit
  )
  def _tdOneRef(ref: Long,
                curEntity: rx.Var[Long],
                mouseover: Long => () => Unit
               )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
    attr("card") := 1,
    ref,
    onmouseover := mouseover(ref),
    noEdit
  )

  def _tdOneStrEdit(cellClass: String,
                    cellId: String,
                    eid: Long,
                    optStr: Option[String],
                    save: () => Unit): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 1,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    _optStr2frags(optStr)
  )

  def _tdOneDateEdit(cellClass: String,
                     cellId: String,
                     eid: Long,
                     optValue: Option[String],
                     save: () => Unit): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 1,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    optValue.fold("")(truncateDateStr(_))
  )

  // T String/Double
  def _tdOneNumEdit[T](cellClass: String,
                       cellId: String,
                       eid: Long,
                       optValue: Option[T],
                       save: () => Unit): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 1,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    optValue.fold("")(_.toString)
  )


  def _tdOneRefEdit(cellId: String,
                    eid: Long,
                    optValue: Option[Double],
                    curEntity: rx.Var[Long],
                    onMouseover: Long => () => Unit,
                    save: () => Unit
                   )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val refStr = optValue.fold("")(_.toString)
    val ref    = optValue.fold(0L)(_.toLong)
    td(
      cls := Rx(if (ref > 0L && ref == curEntity()) "eidChosen" else "eid"),
      refStr,
      onmouseover := onMouseover(ref),
      id := cellId,
      attr("card") := 1,
      attr("eid") := eid,
      contenteditable := true,
      onblur := save,
    )
  }

  def _tdOneRefEdit2(cellId: String,
                     eid: Long,
                     optValue: Option[Double],
                     save: () => Unit
                    )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val ref = optValue.fold("")(_.toString)
    td(
      cls := "num",
      ref,
      id := cellId,
      attr("card") := 1,
      attr("eid") := eid,
      contenteditable := true,
      onblur := save,
    )
  }

  def _tdOneEdit(cellClass: String,
                 cellId: String,
                 eid: Long,
                 optValue: Option[String],
                 save: () => Unit): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 1,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    optValue.fold("")(_.toString)
  )


  // Card many ========================================================


  def _tdManyString(vs: List[String],
                    cellType: String,
                    showAll: Boolean): TypedTag[TableCell] = td(
    cls := cellType,
    attr("card") := 2,
    if (showAll)
      ul(vs.sorted.map(v => li(_str2frags(v))))
    else
      expandingList(vs.sorted.map(v => li(_str2frags(v))), true)
  )

  def _tdManyDate(vs: List[String],
                  showAll: Boolean): TypedTag[TableCell] = td(
    cls := "str",
    attr("card") := 2,
    if (showAll)
      ul(vs.sorted.map(v => li(truncateDateStr(v))))
    else
      expandingList(vs.sorted.map(v => li(truncateDateStr(v))))
  )

  def _tdManyDouble(vs: List[Double],
                    showAll: Boolean): TypedTag[TableCell] = td(
    cls := "num",
    attr("card") := 2,
    if (showAll)
      ul(vs.sorted.map(li(_)))
    else
      expandingList(vs.sorted.map(li(_)))

  )

  def _tdManyRef(vs: List[Double],
                 curEntity: rx.Var[Long],
                 mouseovers: Long => () => Unit,
                 showAll: Boolean = false,
                 max: Int = defaultSize
                )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val vsSorted = vs.sorted
    val list     = ul().render
    def addToList(ref: Long): Node = list.appendChild(
      li(
        cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
        ref,
        onmouseover := mouseovers(ref)
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
      attr("card") := 2,
      list
    )
  }

  def _tdManyStringEdit(vs: List[String],
                        cellClass: String,
                        cellId: String,
                        eid: Long,
                        save: () => Unit): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    ul(vs.sorted.map(v => li(_str2frags(v))))
  )

  def _tdManyDateEdit(vs: List[String],
                      cellClass: String,
                      cellId: String,
                      eid: Long,
                      save: () => Unit): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    ul(vs.sorted.map(v => {
//      println("--------------")
//      println("raw date 2: " + v)
//      println("raw date 2: " + truncateDateStr(v))
//      li(truncateDateStr(v))
      li(v)
    }))
  )

  def _tdManyStringOtherEdit(vs: List[String],
                             cellClass: String,
                             cellId: String,
                             eid: Long,
                             save: () => Unit): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    ul(vs.sorted.map(li(_)))
  )

  def _tdManyStringBigEdit(vs: List[String],
                           cellClass: String,
                           cellId: String,
                           eid: Long,
                           save: () => Unit): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    ul(vs.sorted.map(li(_)))
  )

  def _tdManyDoubleEdit(vs: List[Double],
                        cellClass: String,
                        cellId: String,
                        eid: Long,
                        save: () => Unit): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 2,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    ul(vs.sorted.map(li(_)))
  )

  def _tdManyRefEdit(refs: List[Double],
                     cellId: String,
                     eid: Long,
                     curEntity: rx.Var[Long],
                     mouseovers: Long => () => Unit,
                     save: () => Unit
                    )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    td(
      id := cellId,
      attr("card") := 2,
      attr("eid") := eid,
      contenteditable := true,
      onblur := save,
      ul(
        refs.map(_.toLong).sorted.map(ref =>
          li(
            cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
            ref,
            onmouseover := mouseovers(ref)
          )
        )
      )
    )
  }

  def _tdManyRefEdit2(refs: List[Double],
                      cellClass: String,
                      cellId: String,
                      eid: Long,
                      curEntity: rx.Var[Long],
                      mouseovers: Long => () => Unit,
                      save: () => Unit
                     )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    td(
      cls := cellClass,
      id := cellId,
      attr("card") := 2,
      attr("eid") := eid,
      contenteditable := true,
      onblur := save,
      ul(
        refs.map(_.toLong).sorted.map(ref =>
          li(
            //            cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
            cls := "num",
            ref,
            onmouseover := mouseovers(ref)
          )
        )
      )
    )
  }


  // Map ==============================================================

  def _tdMapEdit[T](cellClass: String,
                    cellId: String,
                    eid: Long,
                    save: () => Unit,
                    pairs: Seq[(String, T)],
                    processPair: (String, T) => Frag
                   ): TypedTag[TableCell] = td(
    cls := cellClass,
    id := cellId,
    attr("card") := 3,
    attr("eid") := eid,
    contenteditable := true,
    onblur := save,
    ul(pairs.map { case (k, v) => li(processPair(k, v)) })
  )

  def _tdMapStrEdit(vs: Map[String, String],
                    cellClass: String,
                    cellId: String,
                    eid: Long,
                    save: () => Unit): TypedTag[TableCell] =
    _tdMapEdit(cellClass, cellId, eid, save,
      vs.toSeq.sortBy(_._1),
      (k: String, v: String) => _str2frags(k + " -> " + v)
    )

  def _tdMapDateEdit(vs: Map[String, String],
                     cellClass: String,
                     cellId: String,
                     eid: Long,
                     save: () => Unit): TypedTag[TableCell] =
    _tdMapEdit(cellClass, cellId, eid, save,
      vs.toSeq.sortBy(_._1),
      (k: String, v: String) => k + " -> " + truncateDateStr(v)
    )

  def _tdMapStrOtherEdit(vs: Map[String, String],
                         cellClass: String,
                         cellId: String,
                         eid: Long,
                         save: () => Unit): TypedTag[TableCell] =
    _tdMapEdit(cellClass, cellId, eid, save,
      vs.toSeq.sortBy(_._1),
      (k: String, v: String) => k + " -> " + v
    )

  def _tdMapDoubleEdit(vs: Map[String, Double],
                       cellClass: String,
                       cellId: String,
                       eid: Long,
                       save: () => Unit): TypedTag[TableCell] =
    _tdMapEdit(cellClass, cellId, eid, save,
      vs.toSeq.sortBy(_._1),
      (k: String, v: Double) => k + " -> " + v
    )


  def _tdMapStr(vs: Map[String, String]): TypedTag[TableCell] =
    mapCell(vs.toSeq.sortBy(_._1), (v: String) => td(_str2frags(v)))(noEdit)

  def _tdMapDate(vs: Map[String, String]): TypedTag[TableCell] =
    mapCell(vs.toSeq.sortBy(_._1), (v: String) => td(
      truncateDateStr(v)
//      v
    ))(noEdit)

  def _tdMapStrOther(vs: Map[String, String]): TypedTag[TableCell] =
    mapCell(vs.toSeq.sortBy(_._1), (v: String) => td(v))(noEdit)

  def _tdMapDouble(vs: Map[String, Double]): TypedTag[TableCell] =
    mapCell(vs.toSeq.sortBy(_._1), (v: Double) => td(v))(noEdit)


  // Hard-coding each combination to make row rendering as fast as possible

  // t ----------------------------------------

  def _tdOneT(t: Long,
              curT: rx.Var[Long],
              mouseover: Long => () => Unit
             )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curT() == t) "txChosen" else "tx"),
    t, onmouseover := mouseover(t), noAggrEdit
  )
  def _tdOneT_tx(t: Long,
                 tx: Long,
                 curT: rx.Var[Long],
                 mouseover: (Long, Long) => () => Unit
                )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curT() == t) "txChosen" else "tx"),
    t, onmouseover := mouseover(t, tx), noAggrEdit
  )
  def _tdOneT_inst(t: Long,
                   txInstant: String,
                   curT: rx.Var[Long],
                   mouseover: (Long, String) => () => Unit
                  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curT() == t) "txChosen" else "tx"),
    t, onmouseover := mouseover(t, txInstant), noAggrEdit
  )
  def _tdOneT_tx_inst(t: Long,
                      tx: Long,
                      txInstant: String,
                      curT: rx.Var[Long],
                      mouseover: (Long, Long, String) => () => Unit
                     )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curT() == t) "txChosen" else "tx"),
    t, onmouseover := mouseover(t, tx, txInstant), noAggrEdit
  )

  // tx ----------------------------------------

  def _tdOneTx(tx: Long,
               curTx: rx.Var[Long],
               mouseover: Long => () => Unit
              )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curTx() == tx) "txChosen" else "tx"),
    tx, onmouseover := mouseover(tx), noAggrEdit
  )
  def _tdOneTx_t(tx: Long,
                 t: Long,
                 curTx: rx.Var[Long],
                 mouseover: (Long, Long) => () => Unit
                )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curTx() == tx) "txChosen" else "tx"),
    tx, onmouseover := mouseover(tx, t), noAggrEdit
  )
  def _tdOneTx_inst(tx: Long,
                    txInstant: String,
                    curTx: rx.Var[Long],
                    mouseover: (Long, String) => () => Unit
                   )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curTx() == tx) "txChosen" else "tx"),
    tx, onmouseover := mouseover(tx, txInstant), noAggrEdit
  )
  def _tdOneTx_t_inst(tx: Long,
                      t: Long,
                      txInstant: String,
                      curTx: rx.Var[Long],
                      mouseover: (Long, Long, String) => () => Unit
                     )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curTx() == tx) "txChosen" else "tx"),
    tx, onmouseover := mouseover(tx, t, txInstant), noAggrEdit
  )

  // txInstant -----------------------------------

  def _tdOneTxInstant(txInstant: String,
                      curTxInstant: rx.Var[String],
                      mouseover: String => () => Unit
                     )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curTxInstant() == txInstant) "txChosen" else "tx"),
    txInstant, onmouseover := mouseover(txInstant), noAggrEdit
  )
  def _tdOneTxInstant_t(txInstant: String,
                        t: Long,
                        curTxInstant: rx.Var[String],
                        mouseover: (String, Long) => () => Unit
                       )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curTxInstant() == txInstant) "txChosen" else "tx"),
    txInstant, onmouseover := mouseover(txInstant, t), noAggrEdit
  )
  def _tdOneTxInstant_tx(txInstant: String,
                         tx: Long,
                         curTxInstant: rx.Var[String],
                         mouseover: (String, Long) => () => Unit
                        )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curTxInstant() == txInstant) "txChosen" else "tx"),
    txInstant, onmouseover := mouseover(txInstant, tx), noAggrEdit
  )
  def _tdOneTxInstant_t_tx(txInstant: String,
                           t: Long,
                           tx: Long,
                           curTxInstant: rx.Var[String],
                           mouseover: (String, Long, Long) => () => Unit
                          )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = td(
    cls := Rx(if (curTxInstant() == txInstant) "txChosen" else "tx"),
    txInstant, onmouseover := mouseover(txInstant, t, tx), noAggrEdit
  )

}
