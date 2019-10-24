package moleculeadmin.shared.ast
import moleculeadmin.shared.ast.schema.seq
import moleculeadmin.shared.ast.tree.Tree
import molecule.ast.model.Element
import moleculeadmin.shared.util.HelpersAdmin


object query extends HelpersAdmin {


  case class QueryResult(str: List[Array[Option[String]]],
                         num: List[Array[Option[Double]]],
                         listStr: List[Array[Option[List[String]]]],
                         listNum: List[Array[Option[List[Double]]]],
                         mapStr: List[Array[Option[Map[String, String]]]],
                         mapNum: List[Array[Option[Map[String, Double]]]],
                         arrayIndexes: Map[Int, Int], // colIndex -> arrayIndex
                         rowCountAll: Int,
                         rowCount: Int,
                         queryMs: Long)


  case class Col(colIndex: Int,
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
                 sortPos: Int = 0) {
    override def toString = {
      s"""Col($colIndex, $related, "$nsAlias", "$nsFull", "$attr", "$attrType", "$colType", $card, $opt, ${seq(enums)}, "$aggrType", "$attrExpr", "$sortDir", $sortPos)"""
    }
  }


  /** Filter data
    *
    * @tparam T Double or String
    * @param colIndex index in table
    * @param filterExpr     filter expression including possible newlines
    * @param pred     Scala predicate to filter each value
    * @param jsCode   JS code to `eval` when no typed Scala predicate
    **/
  case class Filter[T](colIndex: Int,
                       colType: String,
                       filterExpr: String,
                       pred: T => Boolean,
                       jsCode: String = "")


  case class QueryCache(modelElements: Seq[Element],
                        tree: Tree,
                        molecule: String,
                        queryResult: QueryResult,
                        columns: Seq[Col],
                        sortIndex: Array[Int],
                        filters: Map[Int, Filter[_]] = Map.empty[Int, Filter[_]],
                        filterIndex: Array[Int])

  case class Favorite(molecule: String,
                      colSettings: Seq[ColSetting])

  case class ColSetting(index: Int,
                        attrExpr: String,
                        sortDir: String,
                        sortPos: Int)

}
