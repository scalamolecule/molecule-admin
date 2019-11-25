package moleculeadmin.client.app.domain.query
import moleculeadmin.shared.ast.query.{Col, SavedQuery, Filter, QueryCache}
import moleculeadmin.shared.ast.schema.Ns
import moleculeadmin.shared.ast.tree.Tree
import molecule.ast.model.Element
import moleculeadmin.client.app.domain.query.data.RowBuilder
import rx.Var
import scalatags.JsDom.all.s
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object QueryState {

  // Db settings id so that we can avoid looking it up on each marker toggle
  var dbSettingsIdOpt = Option.empty[Long]

  // Schema
  implicit var nsMap: Map[String, Ns] = Map.empty[String, Ns]
  var viewCellTypes    = Map.empty[String, String]
  var enumAttrs        = Seq.empty[String]
  var eTableColIndexes = Seq.empty[Int]


  // What is being being processed now? Concatenates part/ns/attr/pos-number
  // as a coordinate in the tree
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

  // Re-use recent query results
  var queryCache = Var(Seq.empty[QueryCache])

  // Avoiding re-creating sort index array during paging
  var cachedCols       = Seq.empty[Col]
  var cachedSortIndex  = Array.empty[Int]
  var cachedRowBuilder = Option.empty[RowBuilder]

  // Avoiding re-creating index array during paging
  var cachedFilters     = Map.empty[Int, Filter[_]]
  var cachedFilterIndex = Array.empty[Int]

  // Track maxRows change
  var savedMaxRows = -1

  var changeArrays = Map.empty[Int, Array[Int]]


  // Views --------------------------------

  val curMolecule = Var("")

  val savedQueries = Var(Seq.empty[SavedQuery])

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
  val showViews           = Var(false)
  val showHelp            = Var(false)
  val showMolecule        = Var(false)
  val showQueries         = Var(false)
  val showRecentMolecules = Var(false)
  val showDatalog         = Var(false)
  val showTransaction     = Var(false)
  val showEntity          = Var(false)
  val showEntityHistory   = Var(false)

  // Debugging
  val showMoleculeModel = Var(false)
  val showMoleculeQuery = Var(false)
  val showColumns       = Var(false)
  val showTree1         = Var(false)
  val showTree2         = Var(false)
  val showTree3         = Var(false)
}
