package moleculeadmin.client.app.domain.query.keyEvents

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.edit.{Insert, RetractEid}
import moleculeadmin.client.app.domain.query.marker.ToggleOne
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.shared.ops.query.BaseQuery
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{TableCell, TableSection}
import org.scalajs.dom.raw.{HTMLCollection, KeyboardEvent}
import org.scalajs.dom.{document, window}
import rx.Ctx
import scalatags.JsDom.all.{onblur, _}
import scala.concurrent.ExecutionContext.Implicits.global


// https://stackoverflow.com/questions/25389807/how-do-i-undo-or-reverse-a-transaction-in-datomic

trait Undoing  {
  type keepBooPickleImport_Undoing = PickleState


  def toggleUndo(): Unit = {
    println("undo...")
    showUndo() = !showUndo.now
  }


}
