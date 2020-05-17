package moleculeadmin.client.app.logic.query.data

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.data.{FilterIndex, SortIndex}


object IndexBridge extends SortIndex with FilterIndex {

  private def getSortIndex(qr: QueryResult, sortCols: Seq[Col]): Array[Int] =
    getSortIndex(qr, sortCols, rowCount)

  private def getFilterIndex(qr: QueryResult, sortIndex: Array[Int]): Array[Int] =
    getFilterIndex(qr, filters.now, curStars, curFlags, curChecks, sortIndex)


  def apply(qr: QueryResult): Int => Int = {
    val sortCols = columns.now.filter(_.sortDir.nonEmpty)
    val sort     = sortCols.nonEmpty
    val filter   = filters.now.nonEmpty

    val sortFilterIndex =
      if (sort && filter)
        getFilterIndex(qr, getSortIndex(qr, sortCols))
      else if (sort)
        getSortIndex(qr, sortCols)
      else if (filter)
        getFilterIndex(qr, Array.empty[Int])
      else
        Array.empty[Int]

    cachedSortFilterIndex = sortFilterIndex

    if (sortFilterIndex.isEmpty)
      (i: Int) => i
    else
      (i: Int) => sortFilterIndex(i)
  }
}
