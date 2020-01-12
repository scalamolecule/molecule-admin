package moleculeadmin.client.app.domain.query

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.grouped.Grouped
import moleculeadmin.client.app.element.query.GroupedAttrElements
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class RenderUndo()(implicit ctx: Ctx.Owner)
  extends GroupedAttrElements {
  type keepBooPickleImport_RenderUndo = PickleState


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    showUndo()
    if (showUndo.now) {
      var i = 0
      queryWire().getLastTxs(db, 1, enumAttrs).call().foreach { txs =>
        txs.foreach {
//          case (null,_,_,_) =>
//            println("NULL !!!!!!!!!!!!!")
          case (t, tx, date, datoms) =>
            i += 1
            println(s"---- $i ---------------------")
            println(s"$t  $tx  $date")
            datoms.foreach(d => println("  " + d))
        }
      }
      _cardsContainer(
        _card(
          _cardHeader(h5("Undo")),
          _cardBody(
//            padding := 0,
            "Fetching last datoms..."
          )
        )
      )
    } else {
      span()
    }
  }
}
