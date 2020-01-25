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
      println(s"Loading $chunks chunks of latest txs")
      queryWire().getLastTxs(db, chunks, enumAttrs).call().foreach {
        case Right(txs) => setUndoRows(txs)
        case Left(err)  => setUndoRows(Array.empty[TxData], err)
      }
      container
    }
    else {
      span()
    }
  }
}
