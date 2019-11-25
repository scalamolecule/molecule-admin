package moleculeadmin.client.app.domain.query.data
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.data.{FilterIndex, SortIndex}
import rx.Ctx


case class Indexes(
  qr: QueryResult,
  sortCols: Seq[Col],
  unfiltered: Boolean
)(implicit ctx: Ctx.Owner) extends SortIndex with FilterIndex {

  def getCachedSortIndex: Array[Int] =
    if (cachedCols != columns.now) {
      cachedCols = columns.now
      // sort
      cachedSortIndex = getSortIndex(qr, sortCols, rowCount)
      cachedSortIndex
    } else {
      cachedSortIndex
    }

  def getCachedFilterIndex: Array[Int] = {
    if (cachedCols != columns.now || cachedFilters != filters.now) {
      cachedFilters = filters.now
      cachedFilterIndex = if (sortCols.isEmpty) {
        // filter
        getFilterIndex(qr, filters.now)
      } else {
        // sort + filter
        getFilterIndex(qr, filters.now, getCachedSortIndex)
      }
      cachedFilterIndex
    } else {
      cachedFilterIndex
    }
  }

  def get: (Array[Int], Array[Int]) = {
    if (sortCols.isEmpty && unfiltered) {
      // No sort/filter
      (Array.empty[Int], Array.empty[Int])

    } else if (unfiltered) {
      // sort
      (getCachedSortIndex, Array.empty[Int])

    } else {
      // filter or sort+filter
      (Array.empty[Int], getCachedFilterIndex)
    }
  }

  def getIndexBridge: Int => Int = {
    val (sortIndex, filterIndex) = get
    if (filterIndex.nonEmpty)
      (i: Int) => filterIndex(i)
    else if (sortIndex.nonEmpty)
      (i: Int) => sortIndex(i)
    else
      (i: Int) => i
  }
}
