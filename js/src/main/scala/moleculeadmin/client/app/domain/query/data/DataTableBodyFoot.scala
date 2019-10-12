package moleculeadmin.client.app.domain.query.data
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState.{columns, _}
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.{QueryCache, QueryResult}
import org.scalajs.dom.html.TableSection
import rx.{Ctx, Rx}


case class DataTableBodyFoot(db: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with KeyEvents {

  def populate(tableBody: TableSection,
               tableFoot: TableSection,
               optQueryResult: Option[QueryResult] = None): Unit = Rx {
    //    println("---- body ----")

    // triggers
    columns()
    filters()
    offset()
    limit()

    val queryResult              = optQueryResult.getOrElse {
      // println("  cached queryResult")
      queryCache.now.find(_.modelElements == modelElements.now).get.queryResult
    }
    val sortCols                 = columns.now.filter(_.sortDir.nonEmpty)
    val unfiltered               = filters.now.isEmpty
    val (sortIndex, filterIndex) = Indexes(queryResult, sortCols, unfiltered).get

    // Cache current query result and rendering data
    queryCache() = QueryCache(
      modelElements.now,
      tree.now,
      curMolecule.now,
      queryResult,
      columns.now,
      sortIndex,
      filters.now,
      filterIndex
    ) +: queryCache.now.filterNot(_.modelElements == modelElements.now)

    // Populate table body and foot
    tableBody.innerHTML = ""
    RowBuilder(db, tableBody, columns.now, queryResult, sortIndex, filterIndex).append()
    DataTableFoot().populate(tableFoot)
  }
}
