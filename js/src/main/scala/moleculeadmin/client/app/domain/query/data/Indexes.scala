package moleculeadmin.client.app.domain.query.data
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.data.{FilterIndex, SortIndex}
import rx.Ctx


case class Indexes(queryResult: QueryResult,
                   sortCols: Seq[Col],
                   unfiltered: Boolean
                  )(implicit ctx: Ctx.Owner)
  extends SortIndex with FilterIndex {


  def getCachedSortIndex: Array[Int] =
    if (cachedCols != columns.now) {
      cachedCols = columns.now
      // sort
      cachedSortIndex = getSortIndex(queryResult, sortCols, rowCount)
      //      println("## cachedSortIndex: " + cachedSortIndex.toList)
      cachedSortIndex
    } else {
      cachedSortIndex
    }

  def getCachedFilterIndex: Array[Int] = {
    if (cachedCols != columns.now || cachedFilters != filters.now) {
      cachedFilters = filters.now
      cachedFilterIndex = if (sortCols.isEmpty) {
        // filter
        getFilterIndex(queryResult, filters.now)
      } else {
        // sort + filter
        getFilterIndex(queryResult, filters.now, getCachedSortIndex)
      }
      //      println("CREATED: " + cachedFilterIndex.toList)
      cachedFilterIndex
    } else {
      //      println("CACHED: " + cachedFilterIndex.toList)
      cachedFilterIndex
    }
  }

  def get: (Array[Int], Array[Int]) = {
    //    println("## sortCols     : " + sortCols)
    //    println("## unfiltered   : " + unfiltered)
    //    println("## cachedCols   : " + cachedCols)
    //    println("## columns      : " + columns.now)
    //    println("## cachedFilters: " + cachedFilters)
    //    println("## filters      : " + filters.now)

    val (sortIndex, filterIndex) =
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

    //    println("sortIndex  : " + sortIndex.toList)
    //    println("filterIndex: " + filterIndex.toList)

    (sortIndex, filterIndex)
  }
}
