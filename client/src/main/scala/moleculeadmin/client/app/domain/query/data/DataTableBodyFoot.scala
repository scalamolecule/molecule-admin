package moleculeadmin.client.app.domain.query.data
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState.{columns, _}
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.{QueryCache, QueryData, QueryResult}
import moleculeadmin.shared.ops.query.{ColOps, MoleculeOps}
import org.scalajs.dom.html.TableSection
import rx.{Ctx, Rx}


case class DataTableBodyFoot(db: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with KeyEvents with ColOps with MoleculeOps {

  cachedRowBuilder = None
  //  println("---- DataTableBodyFoot ----")

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

    val c = QueryCache(
      modelElements.now,
      tree.now,
      curMolecule.now,
      queryResult,
      columns.now,
      sortIndex,
      filters.now,
      filterIndex,
      showGrouped.now,
      groupedCols.now
    )

    queryCache2 = c

    val (part, ns) = getPartNs(curMolecule.now)

    recentQueries() = QueryData(
      curMolecule.now,
      part, ns,
      false,
      showGrouped.now,
      groupedCols.now,
      colSettings(columns.now)
    ) +: recentQueries.now.filterNot(_.molecule == curMolecule.now)


    // Populate table body and foot
    tableBody.innerHTML = ""
    val rowBuilder = cachedRowBuilder.getOrElse {
      val rb = RowBuilder(db, tableBody, columns.now, queryResult)
      cachedRowBuilder = Some(rb)
      rb
    }

    //    println("append...")

    rowBuilder.append(sortIndex, filterIndex)
    DataTableFoot().populate(tableFoot)
  }
}
