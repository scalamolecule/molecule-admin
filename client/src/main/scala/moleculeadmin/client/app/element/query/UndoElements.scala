package moleculeadmin.client.app.element.query

import moleculeadmin.client.app.domain.query.QueryState.{curEntity, curTx, new2undone, undone2new}
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.html._
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


trait UndoElements extends AppElements with RxBindings {

  val datomTable =
    table(
      cls := "undoTxs",
      id := "undoTxs",
      tr(td("Fetching tx data from Log..."))
    ).render

  val container =
    _cardsContainer(
      _card(
        _cardHeader(h5("Undo")),
        _cardBody(
          padding := 0,
          datomTable
        )
      )
    )

  def _loadMoreRow(
    err: String,
    loadMore: () => Unit
  ): TableRow =
    tr(
      td(
        colspan := 3,
        paddingBottom := 5,
        if (err.isEmpty)
          a(href := "#", "Load more...", onclick := loadMore)
        else
          err
      )
    ).render


  def _headerRow(
    t: Long,
    tx: Long,
    isUndone: Boolean,
    canUndo: Boolean,
    notLast: Boolean,
    setTx: () => Unit,
    highlightUndoneT: () => Unit,
    highlightNewT: () => Unit,
    undoAllNext: () => Unit,
    undoCleanNext: () => Unit,
    undoThis: () => Unit
  )(implicit ctx: Ctx.Owner): TableRow = {
    val undone = if(isUndone) " undone" else ""
    tr(
      cls := Rx(if (tx == curTx()) "header  chosen" else "header" + undone),
      onmouseover := setTx,
      td(
        if (new2undone.contains(t))
          span(s(new2undone(t)), onmouseover := highlightUndoneT, br)
        else
          span(),

        if (undone2new.contains(t))
          span(s(t), " --> ", span(undone2new(t), onmouseover := highlightNewT))
        else
          span(t)
      ),
      td(
        colspan := 2,
        textAlign.right,
        verticalAlign.bottom,
        if (canUndo) {
          span(
            if (notLast) {
              span(
                a(href := "#", s"Undo next", onclick := undoAllNext),
                a(href := "#", s"Undo next clean", onclick := undoCleanNext)
              )
            } else (),
            a(href := "#", "Undo this", onclick := undoThis)
          )
        } else {
          span()
        }
      )
    ).render
  }

  def _txInstantRow(
    tx: Long,
    txInstant: String,
    isUndone: Boolean,
    setTx: () => Unit
  )(implicit ctx: Ctx.Owner): TableRow = {
    val undone = if(isUndone) " undone" else ""
    tr(
      cls := Rx(if (tx == curTx()) "txMetaData chosen" else "txMetaData" + undone),
      onmouseover := setTx,
      td(tx),
      td(":db/txInstant"),
      td(txInstant),
    ).render
  }

  def _txMetaDataRow(
    tx: Long,
    e: Long,
    a: String,
    v: String,
    isUndone: Boolean,
    setTx: () => Unit
  )(implicit ctx: Ctx.Owner): TableRow = {
    val undone = if(isUndone) " undone" else ""
    tr(
      cls := Rx(if (tx == curTx()) "txMetaData chosen" else "txMetaData" + undone),
      onmouseover := setTx,
      td(e),
      td(a),
      td(v),
    ).render
  }

  def _txDataRow(
    tx: Long,
    e: Long,
    isUndone: Boolean,
    setTx: () => Unit,
    showEntity: Boolean,
    highlightEntity: () => Unit,
    attrCell: TypedTag[TableCell],
    valueCell: TypedTag[TableCell]
  )(implicit ctx: Ctx.Owner): TableRow = {
    val undone = if(isUndone) " undone" else ""
    tr(
      cls := Rx(if (tx == curTx()) "chosen" else undone),
      onmouseover := setTx,
      if (showEntity)
        td(
          e,
          cls := Rx(if (e == curEntity()) "eid chosen" else "eid"),
          onmouseover := highlightEntity
        )
      else
        td(),
      attrCell,
      valueCell
    ).render
  }

  def _txDataMoreRow(
    tx: Long,
    isUndone: Boolean,
    more: String
  )(implicit ctx: Ctx.Owner): TableRow = {
    val undone = if(isUndone) " undone" else ""
    tr(
      cls := Rx(if (tx == curTx()) "chosen" else undone),
      td(
        colspan := 3,
        fontStyle.italic,
        more
      )
    ).render
  }
}
