package moleculeadmin.client.app.domain.query

import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.ViewElements
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


case class GroupedRender(db: String)(implicit val ctx: Ctx.Owner)
  extends Callbacks
    with ViewElements with ModelOps with ColOps with TreeOps {
  type keepBooPickleImport_ViewsRender = PickleState


  def groupData(
    qr: QueryResult,
    colIndex: Int,
    colType                 : String
  ): Seq[(String, Int)] = {
    colType match {
      case "string" =>
        qr.str(qr.arrayIndexes(colIndex)).toList.map {
          case None                      => "<nil>"
          case Some(s) if s.trim.isEmpty => s"{$s}"
          case Some(s)                   => s
        }.groupBy(identity).mapValues(_.length).toSeq
          .sortBy { case (v, c) => (-c, v) }

      //      case "double" =>
      //        val valueArray = qr.num(qr.arrayIndexes(colIndex))
    }


  }


  def rxElement: Rx.Dynamic[TypedTag[Element]] = Rx {
    if (groupedCols().nonEmpty && showGrouped()) {
      val qr: QueryResult = queryCache2.queryResult
//      val qr: QueryResult = queryCache.now
//        .find(_.modelElements == modelElements.now).get.queryResult

      _cardsContainer(
        groupedCols.now.map { colIndex =>
          val Col(_, _, nsAlias, _, attr, _, colType, _, _, _, _, _, _, _, _) =
            columns.now.find(_.colIndex == colIndex).get
          _grouped(
            s"$nsAlias $attr",
            _groupedTable(groupData(qr, colIndex, colType))
          )
        }
      )
    } else span()
  }
}
