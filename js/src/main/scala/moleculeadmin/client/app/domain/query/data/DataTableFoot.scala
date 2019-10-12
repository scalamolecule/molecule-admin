package moleculeadmin.client.app.domain.query.data
import moleculeadmin.client.app.element.query.datatable.FootElements
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{TableRow, TableSection}
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
    }
    val footRow = tr(
      td(
        colspan := 100,
        _firstPage(isFirst)(onclick := { () => firstPage }),
        _prevPage(isFirst)(onclick := { () => prevPage }),
        limitSelector,
        _nextPage(isLast)(onclick := { () => nextPage }),
        _lastPage(isLast)(onclick := { () => lastPage }),
        _rightSpace((offset.now + 1) + "-" + curLastRow, 7),
        _rightSpace("of", 7),
        _rightSpace(thousands(actualRowCount), 7),

        if (actualRowCount != rowCount &&
          maxRows.now != -1 &&
          rowCountAll > maxRows.now) {
          _rightSpace("/ " + thousands(rowCount) +
            " (" + thousands(rowCountAll) + " in total)", 7)
        } else if (actualRowCount != rowCount) {
          _rightSpace("/ " + thousands(rowCount), 7)
        } else ()
      )
    )

    tableFoot.innerHTML = ""
    tableFoot.appendChild(footRow.render)
  }
}
