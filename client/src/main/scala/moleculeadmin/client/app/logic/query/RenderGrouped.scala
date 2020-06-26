package moleculeadmin.client.app.logic.query

import moleculeadmin.client.app.html.query.GroupedAttrElements
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.grouped.Grouped
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


case class RenderGrouped()(implicit ctx: Ctx.Owner)
  extends GroupedAttrElements {


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    groupedColIndexes()
    if (showGrouped && groupedColIndexes.now.nonEmpty) {
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
