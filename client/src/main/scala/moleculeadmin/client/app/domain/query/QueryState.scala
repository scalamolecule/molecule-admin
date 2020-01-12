package moleculeadmin.client.app.domain.query
import moleculeadmin.shared.ast.query.{Col, QueryDTO, Filter, QueryCache}
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

  // Schema ----------------------------------------
  implicit var nsMap: Map[String, Ns] = Map.empty[String, Ns]
  var viewCellTypes = Map.empty[String, String]
  var enumAttrs     = Seq.empty[String]
  var eidCols       = Seq.empty[Int]

  // Tree branches ---------------------------------
  val tree = Var(Tree(Nil, Nil, Nil, Nil, Nil, Nil))

  // Current processing coordinate in tree (part/ns/attr/pos-number)
  val processing = Var[String]("")

  // Query building ---------------------------------
  var newQueryBuildup = true

  // Toggling
  val querySelection     = Var("a")
  var queryMinimized     = false
  var queryBaseSelection = "a"

  // Data ------------------------------------------------------

  val modelElements = Var(Seq.empty[Element])
  val curMolecule   = Var("")
  val columns       = Var(Seq.empty[Col])

  // Filters indexed by colIndex
  val filters = Var(Map.empty[Int, Filter[_]])

  // Table foot
  var rowCountAll = 0
  var rowCount    = 0
  val maxRows     = Var(-1)
  val offset      = Var(0)
  val limit       = Var(5)

  var editCellId = ""

  // Caching (for paging) -----------------------------------
  var queryCache: QueryCache = null
  var savedQueries           = Seq.empty[QueryDTO]
  var recentQueries          = Seq.empty[QueryDTO]
  var cachedCols             = Seq.empty[Col]
  var cachedSortIndex        = Array.empty[Int]
  var cachedFilters          = Map.empty[Int, Filter[_]]
  var cachedFilterIndex      = Array.empty[Int]

  val renderSubMenu = Var("trigger")

  // Undo --------------------------------
  var showUndo = Var(false)

  // Grouped --------------------------------
  var showGrouped       = false
  var groupableCols     = Seq.empty[Col]
  val groupedColIndexes = Var(Set.empty[Int])

  // Views --------------------------------
  var showViews = false
  val allViews  = Seq(
    ("view01_Molecule", "Molecule"),
    ("view02_Datalog", "Datalog"),
    ("view03_Entity", "Entity"),
    ("view04_EntityHistory", "Entity History"),
    ("view05_Transaction", "Transaction"),
    ("view06_MoleculeModel", "Molecule Model"),
    ("view07_MoleculeQuery", "Molecule Query"),
    ("view08_Columns", "Columns"),
    ("view09_Tree1", "Tree with attr names only"),
    ("view10_Tree2", "Tree with attr definitions"),
    ("view11_Tree3", "Full Tree")
  )
  val curViews  = Var(Seq.empty[String])


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
}
