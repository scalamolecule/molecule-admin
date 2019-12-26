package moleculeadmin.client.app.domain.query.data

import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.{Callbacks, KeyEvents}
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.{QueryCache, QueryResult}
import moleculeadmin.shared.ops.query.{ColOps, MoleculeOps}
import org.scalajs.dom.html.TableSection
import rx.{Ctx, Rx}


case class DataTableBodyFoot()(implicit val ctx: Ctx.Owner)
  extends Callbacks with RxBindings with KeyEvents with ColOps with MoleculeOps {

  // Locally cached RowBuilder to avoid re-creation on paging
  var cachedRowBuilder = Option.empty[RowBuilder]

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

    val sortCols                 = columns.now.filter(_.sortDir.nonEmpty)
    val unfiltered               = filters.now.isEmpty
    val (sortIndex, filterIndex) = Indexes(queryResult, sortCols, unfiltered).get

    // Cache current query result and rendering data
    queryCache = QueryCache(
      modelElements.now,
      tree.now,
      curMolecule.now,
      queryResult,
      columns.now,
      sortIndex,
      filters.now,
      filterIndex
    )

    // Grouped
    savedQueries.find(_.molecule == curMolecule.now).fold {
      showGrouped = false
      groupedColIndexes() = Set.empty[Int]
    } { query =>
      showGrouped = query.showGrouped
      if (groupedColIndexes.now == query.groupedColIndexes)
        groupedColIndexes.recalc()
      else
        groupedColIndexes() = query.groupedColIndexes
    }
    renderSubMenu.recalc()

    recentQueries =
      curQuery +: recentQueries.filterNot(_.molecule == curMolecule.now)

    // Populate table body and foot
    tableBody.innerHTML = ""
    val rowBuilder = cachedRowBuilder.getOrElse {
      val rb = RowBuilder(tableBody, columns.now, queryResult)
      cachedRowBuilder = Some(rb)
      rb
    }

    rowBuilder.append(sortIndex, filterIndex)
    DataTableFoot().populate(tableFoot)
  }
}
