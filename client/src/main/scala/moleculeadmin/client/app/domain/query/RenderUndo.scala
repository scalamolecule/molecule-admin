package moleculeadmin.client.app.domain.query

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.keyEvents.Undoing
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class RenderUndo()(implicit ctx: Ctx.Owner) extends Undoing {

  type keepBooPickleImport_RenderUndo = PickleState


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    showUndo()
    if (showUndo.now) {
      queryWire().getLastTxs(
        db, curFirstT, groupEdits, enumAttrs
      ).call().foreach {
        case Right(txs) =>
          // caching txs to be accessible by cmd-z undoing shortcut too
          curLastTxs = txs
          curFirstT = txs.headOption.fold(0L)(_._1) // could be empty db
          populateUndoRows()
        case Left(err)  =>
          curLastTxs = Array.empty[TxData]
          curFirstT = 0L
          populateUndoRows(err)
      }
      container
    }
    else {
      span()
    }
  }
}
