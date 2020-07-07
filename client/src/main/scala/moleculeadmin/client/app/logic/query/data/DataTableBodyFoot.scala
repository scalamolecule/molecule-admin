package moleculeadmin.client.app.logic.query.data

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.{Callbacks, KeyEvents}
import moleculeadmin.shared.ast.query.QueryResult
import moleculeadmin.shared.ops.query.{ColOps, MoleculeOps}
import org.scalajs.dom.html.TableSection
import rx.{Ctx, Rx}


case class DataTableBodyFoot()(implicit val ctx: Ctx.Owner)
  extends Callbacks with KeyEvents with ColOps with MoleculeOps {

  // Local cache for paging
  private var cachedRowBuilder: Option[RowBuilder] = Option.empty

  def populate(
    tableBody: TableSection,
    tableFoot: TableSection,
    queryResult: QueryResult
  ): Unit = Rx {
    //    println("--- body/foot ---")

    // triggers
    columns()
    filters()
    offset()
    limit()

    // Force re-calculation of index bridge when columns/filters have changed or
    // new rows have been inserted.
    // Otherwise use cached index bridge to avoid re-calculating when paging.
    if (cachedColumns != columns.now || cachedFilters != filters.now) {
      cachedColumns = columns.now
      cachedFilters = filters.now
      cachedIndexBridge = IndexBridge(queryResult)
    }
    cachedQueryResult = queryResult

    // Populate table body and foot
    tableBody.innerHTML = ""
    val rowBuilder = cachedRowBuilder.getOrElse {
      cachedRowBuilder = Some(RowBuilder(tableBody, columns.now, queryResult))
      cachedRowBuilder.get
    }
    rowBuilder.appendRows()
    DataTableFoot().populate(tableFoot)
  }
}
