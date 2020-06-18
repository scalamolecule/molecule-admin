package moleculeadmin.client.app.logic.query.data

import boopickle.Default._
import util.client.rx.RxBindings
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.{Callbacks, KeyEvents}
import moleculeadmin.client.app.html.query.datatable.FootElements
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.Node
import rx.{Ctx, Rx}
import scalatags.JsDom.all._


case class DataTableFoot()(implicit val ctx: Ctx.Owner)
  extends RxBindings with ColOps with FootElements with KeyEvents {

  def populate(tableFoot: TableSection): Rx.Dynamic[Node] = Rx {
    //    println("---- foot ----")
    val limitSelector = _limitSelector(limit.now)
    limitSelector.onchange = _ => {
      // Let only limit() propagate change
      offset.kill()
      offset() = 0
      limit() = limitSelector.value.toInt
      new Callbacks().saveSetting("limit" -> limit.now.toString)
    }
    val actualCount = actualRowCount
    val lastRow     = curLastRow
    val footRow     = tr(
      td(
        colspan := 100,

        _firstPage(isFirstPage)(onclick := { () => firstPage }),
        _prevPage(isFirstPage)(onclick := { () => prevPage }),
        limitSelector,
        _nextPage(isLastPage)(onclick := { () => nextPage }),
        _lastPage(isLastPage)(onclick := { () => lastPage }),

        if (lastRow == 0)
          "0 " else _rightSpace(s"${offset.now + 1}-$lastRow", 7),

        _rightSpace("of", 7),

        _rightSpace(thousands(actualCount), 12),
        if (maxRows.now != -1 && rowCountAll > maxRows.now) {
          _rightSpace(" ( " + thousands(rowCountAll) + " in total )", 7)
        } else if (actualCount != rowCount) {
          _rightSpace("/ " + thousands(rowCount), 7)
        } else ()
      )
    )

    tableFoot.innerHTML = ""
    tableFoot.appendChild(footRow.render)
  }
}
