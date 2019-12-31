package moleculeadmin.client.app.domain.query

import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.grouped.{Grouped, GroupedData}
import moleculeadmin.client.app.element.query.GroupedAttrElements
import org.scalajs.dom.document
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.scalajs.js.timers.setTimeout


case class RenderGrouped()(implicit ctx: Ctx.Owner)
  extends GroupedAttrElements {


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    groupedColIndexes()
    if (showGrouped && groupedColIndexes.now.nonEmpty) {
      val qr = queryCache.queryResult
      _cardsContainer(
        columns.now.collect {
          case col if groupedColIndexes.now.contains(col.colIndex)
            && groupableCols.map(_.colIndex).contains(col.colIndex) => {
            if (col.colType == "double")
              Grouped[Double](col).render
            else
              Grouped[String](col).render
          }
        }
      )
    } else {
      span()
    }
  }
}
