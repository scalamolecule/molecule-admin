package moleculeadmin.client.app.domain.query
import moleculeadmin.shared.ast.query.{Col, QueryData, Filter, QueryCache}
import moleculeadmin.shared.ast.schema.Ns
import moleculeadmin.shared.ast.tree.Tree
import molecule.ast.model.Element
import moleculeadmin.client.app.domain.query.data.RowBuilder
import rx.Var
import scalatags.JsDom.all.s
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object QueryState {

  var db = ""

  // Db settings id so that we can avoid looking it up on each marker toggle
  var dbSettingsIdOpt = Option.empty[Long]

  // Schema
  implicit var nsMap: Map[String, Ns] = Map.empty[String, Ns]
  var viewCellTypes = Map.empty[String, String]
  var enumAttrs     = Seq.empty[String]
  var eidCols       = Seq.empty[Int]


  // Current processing coordinate in tree (part/ns/attr/pos-number)
  val processing = Var[String]("")

  // Query building ---------------------------------

  var newQueryBuildup = true

  // Tree branches
  val tree = Var(Tree(Nil, Nil, Nil, Nil, Nil, Nil))

  // Toggling
  val builderSelection     = Var("a")
  var builderMinimized     = false
  var builderBaseSelection = "a"


  // Data ------------------------------------------------------

  // Model
  val modelElements = Var(Seq.empty[Element])

  // Column headers
  val columns = Var(Seq.empty[Col])

  // Filters indexed by colIndex
  val filters = Var(Map.empty[Int, Filter[_]])

  // Table foot
  var rowCountAll = 0
  var rowCount    = 0
  val maxRows     = Var(-1)
  val offset      = Var(0)
  val limit       = Var(5)

  var editCellId = ""


  // Caching -----------------------------------

  // Avoiding re-creating sort index array during paging
  var queryCache: QueryCache = null
  var cachedCols             = Seq.empty[Col]
  var cachedSortIndex        = Array.empty[Int]
  var cachedRowBuilder       = Option.empty[RowBuilder]
  var cachedFilters          = Map.empty[Int, Filter[_]]
  var cachedFilterIndex      = Array.empty[Int]

  var changeArrays = Map.empty[Int, Array[Int]]

  val savedQueries  = Var(Seq.empty[QueryData])
  val recentQueries = Var(Seq.empty[QueryData])


  // Views --------------------------------

  val curMolecule = Var("")

  // Entities
  val curEntity         = Var(0L)
  var curAttrs          = Seq.empty[String]
  val entityHistorySort = Var("tx")

  // tableCol = colIndex + 1
  // tableCol -> (eid -> array indexes)
  val curEntityIndexes = mutable.Map.empty[Int, mutable.LongMap[List[Int]]]

  // For fast render on page scrolling
  // tableCol -> array of marker status
  val curStarIndexes  = mutable.Map.empty[Int, Array[Boolean]]
  val curFlagIndexes  = mutable.Map.empty[Int, Array[Boolean]]
  val curCheckIndexes = mutable.Map.empty[Int, Array[Boolean]]

  // Marker On-values
  var curStars  = Set.empty[Long]
  var curFlags  = Set.empty[Long]
  var curChecks = Set.empty[Long]

  // Transactions
  val curTxD       = Var((0L, 0L, ""))
  val curT         = Var(0L)
  val curTx        = Var(0L)
  val curTxInstant = Var("")

  // Show view statuses
  val viewsOn           = Var(false)
  val viewHelp          = Var(false)
  val viewMolecule      = Var(false)
  val viewDatalog       = Var(false)
  val viewTransaction   = Var(false)
  val viewEntity        = Var(false)
  val viewEntityHistory = Var(false)

  // Debugging
  val viewMoleculeModel = Var(false)
  val viewMoleculeQuery = Var(false)
  val viewColumns       = Var(false)
  val viewTree1         = Var(false)
  val viewTree2         = Var(false)
  val viewTree3         = Var(false)

  // Col indexes for grouped attributes
  var groupableCols = Seq.empty[Int]
  val showGrouped   = Var(false)
  val groupedCols   = Var(Set.empty[Int])
}
