package moleculeadmin.shared.ast

import moleculeadmin.shared.ast.schema.seq
import moleculeadmin.shared.ast.tree.Tree
import molecule.ast.model.Element
import moleculeadmin.shared.util.HelpersAdmin


object query extends HelpersAdmin {


  case class QueryResult(
    str: List[Array[Option[String]]],
    num: List[Array[Option[Double]]],
    listStr: List[Array[Option[List[String]]]],
    listNum: List[Array[Option[List[Double]]]],
    mapStr: List[Array[Option[Map[String, String]]]],
    mapNum: List[Array[Option[Map[String, Double]]]],
    arrayIndexes: Map[Int, Int], // colIndex -> index of arrayIndex
    rowCountAll: Int,
    rowCount: Int,
    queryMs: Long
  )


  case class Col(
    colIndex: Int,
    related: Int,
    nsAlias: String,
    nsFull: String,
    attr: String,
    attrType: String,
    colType: String,
    card: Int,
    opt: Boolean = false,
    enums: Seq[String] = Nil,
    aggrType: String = "",
    attrExpr: String = "",
    sortDir: String = "",
    sortPos: Int = 0,
    filterExpr: String = ""
  ) {
    override def toString = {
      s"""Col($colIndex, $related, "$nsAlias", "$nsFull", "$attr", "$attrType", "$colType", $card, $opt, ${seq(enums)}, "$aggrType", "$attrExpr", "$sortDir", $sortPos, "$filterExpr")"""
    }
  }

  // Starred/flagged/checked eids
  type Markers = (Set[Long], Set[Long], Set[Long])

  /** Filter data
   *
   * @tparam T Double or String
   * @param colIndex   Column index
   * @param filterExpr Filter expression including possible newlines
   * @param markerPred Predicate to filter each value.
   *                   Currently starred/flagged/checked eids
   *                   taken as input before applying actual predicate
   * */
  case class Filter[T](
    colIndex: Int,
    colType: String,
    isAggr: Boolean,
    filterExpr: String,
    markerPred: Markers => Option[T] => Boolean
  )

  case class ColSetting(
    colIndex: Int,
    sortDir: String,
    sortPos: Int,
    filterExpr: String
  )

  case class QueryDTO(
    molecule: String,
    part: String,
    ns: String,
    isFavorite: Boolean,
    showGrouped: Boolean,
    groupedColIndexes: Set[Int],
    colSettings: Seq[ColSetting]
  )
}
