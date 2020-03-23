package moleculeadmin.client.app.logic.query

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.keyEvents.Undoing
import moleculeadmin.client.queryWireAjax
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class RenderUndo()(implicit ctx: Ctx.Owner) extends Undoing {


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    showUndo()
    if (showUndo.now) {
      queryWireAjax().getLastTxs(
        db, curFirstT, enumAttrs
      ).call().foreach {
        case Right(txResult) =>
          // caching txs to be accessible by cmd-z undoing shortcut too
          curLastTxResults = txResult
          curFirstT = txResult.headOption.fold(0L)(_._1) // could be empty db
          populateUndoRows()
        case Left(err)       =>
          curLastTxResults = Array.empty[TxResult]
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
