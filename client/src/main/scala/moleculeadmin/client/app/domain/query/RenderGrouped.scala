package moleculeadmin.client.app.domain.query

import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.Cell
import moleculeadmin.client.app.element.query.GroupedAttrElements
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import org.scalajs.dom.html.{Element, TableSection}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.timers.setTimeout


case class RenderGrouped()(implicit ctx: Ctx.Owner)
  extends GroupedAttrElements {


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    groupedColIndexes()
    if (showGrouped && groupedColIndexes.now.nonEmpty) {
      val qr = queryCache.queryResult
      _cardsContainer(
        columns.now.collect {
          case c if groupedColIndexes.now.contains(c.colIndex)
            && groupableCols.map(_.colIndex).contains(c.colIndex) =>
            val grouped   = span().render
            val gd        = GroupedData(qr, c, grouped)
            val data      = gd.getData
            val count     = data.length
            val tableBody = _groupedTableBody(c.colType, count)

            if (count > 10) {
              // Render first 10 immediately
              gd.appendRows(tableBody, data, 0, 10)
              // Render the rest afterwards in the background
              setTimeout(200) {
                gd.appendRows(tableBody, data, 10, count)
              }
            } else {
              gd.appendRows(tableBody, data, 0, count)
            }

            _groupedCard(
              s"${c.nsFull}/${c.attr}",
              grouped,
              _groupedTable(tableBody)
            )
        }
      )
    } else {
      span()
    }
  }
}
