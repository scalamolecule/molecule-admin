package moleculeadmin.client.app.logic.query.data

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.{Callbacks, KeyEvents}
import moleculeadmin.shared.ast.query.{Col, Filter, QueryCache, QueryResult}
import moleculeadmin.shared.ops.query.{ColOps, MoleculeOps}
import org.scalajs.dom.html.TableSection
import rx.{Ctx, Rx}


case class DataTableBodyFoot()(implicit val ctx: Ctx.Owner)
  extends Callbacks with KeyEvents with ColOps with MoleculeOps {

  // Local cache to avoid re-creation on paging
  var cachedColumns     = Seq.empty[Col]
  var cachedFilters     = Map.empty[Int, Filter[_]]
  var cachedSortIndex   = Array.empty[Int]
  var cachedFilterIndex = Array.empty[Int]
  var cachedRowBuilder  = Option.empty[RowBuilder]

  def populate(
    tableBody: TableSection,
    tableFoot: TableSection,
    queryResult: QueryResult
  ): Unit = Rx {
    //    println("---- body ----")

    // triggers
    columns()
    filters()
    offset()
    limit()

    if (cachedColumns != columns.now || cachedFilters != filters.now) {
      cachedColumns = columns.now
      cachedFilters = filters.now

      val (sortIndex, filterIndex) = Indexes(
        queryResult,
        columns.now.filter(_.sortDir.nonEmpty),
        filters.now.isEmpty
      ).get
      cachedSortIndex = sortIndex
      cachedFilterIndex = filterIndex

      // Cache current query result and rendering data
      queryCache = QueryCache(
        modelElements.now,
        tree.now,
        curMolecule.now,
        queryResult,
        columns.now,
        cachedSortIndex,
        filters.now,
        cachedFilterIndex
      )
    }

    // Populate table body and foot
    tableBody.innerHTML = ""
    val rowBuilder = cachedRowBuilder.getOrElse {
      cachedRowBuilder = Some(RowBuilder(tableBody, columns.now, queryResult))
      cachedRowBuilder.get
    }
    rowBuilder.append(cachedSortIndex, cachedFilterIndex)
    DataTableFoot().populate(tableFoot)
  }
}
