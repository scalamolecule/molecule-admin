package moleculeadmin.client.app.domain.query.data.edit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState.{db, modelElements}
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.window
import rx.Ctx
import scala.concurrent.ExecutionContext.Implicits.global


object RetractEid {

  type keepBooPickleImport_UpdateCardOne = PickleState

  def apply(eid: Long)(implicit ctx: Ctx.Owner): Unit = {
    queryWire().retractEntity(db, eid).call().foreach {
      case Right(tx) =>
        println("Retracted entity " + eid)
        modelElements.recalc()

      case Left(err) =>
        val msg = s"Error retracting entity $eid: " + err
        println(msg)
        window.alert(msg)
    }
  }
}
