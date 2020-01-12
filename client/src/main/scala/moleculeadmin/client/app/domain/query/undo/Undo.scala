package moleculeadmin.client.app.domain.query.undo

import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.TypeValidation
import moleculeadmin.client.app.element.query.GroupedAttrElements
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.data.FilterFactory
import org.scalajs.dom.document
import org.scalajs.dom.html.{Element, TableRow, TableSection}
import rx.Ctx
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.scalajs.js.timers.setTimeout


case class Undo[T](col: Col)
  (implicit ctx: Ctx.Owner)
  extends KeyEvents with FilterFactory with TypeValidation {


}
