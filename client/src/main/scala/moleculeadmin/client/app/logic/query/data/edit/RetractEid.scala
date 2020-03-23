package moleculeadmin.client.app.logic.query.data.edit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState.{db, modelElements}
import moleculeadmin.client.queryWireAjax
import org.scalajs.dom.window
import rx.Ctx
import scala.concurrent.ExecutionContext.Implicits.global


object RetractEid {

  def apply(eid: Long)(implicit ctx: Ctx.Owner): Unit = {
    queryWireAjax().retractEntities(db, Array(eid)).call().foreach {
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
