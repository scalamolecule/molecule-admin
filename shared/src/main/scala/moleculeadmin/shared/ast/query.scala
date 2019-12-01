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
      s"""Col($colIndex, $related, "$nsAlias", "$nsFull", "$attr", "$attrType", "$colType", $card, $opt, ${seq(enums)}, "$aggrType", "$attrExpr", "$sortDir", $sortPos)"""
    }
  }


  /** Filter data
   *
   * @tparam T Double or String
   * @param colIndex   Column index
   * @param filterExpr Filter expression including possible newlines
   * @param pred       Scala predicate to filter each value
   **/
  case class Filter[T](
    colIndex: Int,
    colType: String,
    filterExpr: String,
    pred: T => Boolean
  )


  case class QueryCache(
    modelElements: Seq[Element],
    tree: Tree,
    molecule: String,
    queryResult: QueryResult,
    columns: Seq[Col],
    sortIndex: Array[Int],
    filters: Map[Int, Filter[_]] = Map.empty[Int, Filter[_]],
    filterIndex: Array[Int],
    showGrouped: Boolean,
    groupedCols: Seq[Int]
  )

  case class SavedQuery(
    molecule: String,
    colSettings: Seq[ColSetting],
    showGrouped: Boolean,
    groupedCols: Seq[Int]
  )

  case class ColSetting(
    colIndex: Int,
    attrExpr: String,
    sortDir: String,
    sortPos: Int,
    filterExpr: String
  )
}
