package moleculeadmin.client.app.logic.query

import molecule.ast.model.Element
import moleculeadmin.shared.api.QueryApi
import moleculeadmin.shared.ast.query.{Col, Filter, QueryCache, QueryDTO}
import moleculeadmin.shared.ast.schema.{MetaSchema, Ns}
import moleculeadmin.shared.ast.tree.Tree
import rx.Var
import scala.collection.mutable


object QueryState extends QueryApi {

  var db = ""

  var pushUrlOntoHistoryStack = true

  // Db settings id so that we can avoid looking it up on each marker toggle
  var dbSettingsIdOpt = Option.empty[Long]

  // Schema ----------------------------------------
  implicit var nsMap: Map[String, Ns] = Map.empty[String, Ns]
  var metaSchema   : MetaSchema          = null
  var valuesCounted: Boolean             = false
  var viewCellTypes: Map[String, String] = Map.empty
  var enumAttrs    : Seq[String]         = Seq.empty
  var eidCols      : Seq[Int]            = Seq.empty

  // Tree branches ---------------------------------
  val tree: Var[Tree] = Var(Tree(Nil, Nil, Nil, Nil, Nil, Nil))

  // Current processing coordinate in tree (part/ns/attr/pos-number)
  val processing: Var[String] = Var[String]("")

  // Query building ---------------------------------
  var newQueryBuildup: Boolean = true

  // Toggling
  val querySelection    : Var[String] = Var("a")
  var queryMinimized    : Boolean     = false
  var queryBaseSelection: String      = "a"

  // Data ------------------------------------------------------

  val modelElements: Var[Seq[Element]] = Var(Seq.empty[Element])
  val curMolecule  : Var[String]       = Var("")
  val columns      : Var[Seq[Col]]     = Var(Seq.empty[Col])

  // Filters indexed by colIndex
  val filters: Var[Map[Int, Filter[_]]] = Var(Map.empty[Int, Filter[_]])

  // Edit expressions by full attr name -> (order, expr)
  val editExprs: mutable.Map[String, List[String]] = mutable.Map.empty

  // Table foot
  var rowCountAll: Int      = 0
  var rowCount   : Int      = 0
  val maxRows    : Var[Int] = Var(-1)
  val offset     : Var[Int] = Var(0)
  val limit      : Var[Int] = Var(5)

  var editCellId = ""

  // Caching (for paging) -----------------------------------
  var queryCache       : QueryCache          = null
  var savedQueries     : Seq[QueryDTO]       = Seq.empty
  var recentQueries    : Seq[QueryDTO]       = Seq.empty
  var cachedCols       : Seq[Col]            = Seq.empty
  var cachedSortIndex  : Array[Int]          = Array.empty
  var cachedFilters    : Map[Int, Filter[_]] = Map.empty
  var cachedFilterIndex: Array[Int]          = Array.empty
  var cachedIndexBridge: Option[Int => Int]  = None

  val renderSubMenu: Var[String] = Var("trigger")

  // Undo --------------------------------
  var showUndo: Var[Boolean] = Var(false)

  // Grouped --------------------------------
  var showGrouped      : Boolean       = false
  var groupableCols    : Seq[Col]      = Seq.empty
  val groupedColIndexes: Var[Set[Int]] = Var(Set.empty[Int])

  // Views --------------------------------
  var showViews                  = false
  val allViews                   = Seq(
    ("view01_Molecule", "Molecule"),
    ("view02_Datalog", "Datalog"),
    ("view03_Entity", "Entity"),
    ("view04_EntityHistory", "Entity History"),
    ("view05_Transaction", "Transaction"),
    ("view06_Url", "Live Url"),
    ("view07_MoleculeModel", "Molecule Model"),
    ("view08_MoleculeQuery", "Molecule Query"),
    ("view09_Columns", "Columns"),
    ("view10_Tree1", "Tree with attr names only"),
    ("view11_Tree2", "Tree with attr definitions"),
    ("view12_Tree3", "Full Tree")
  )
  val curViews: Var[Seq[String]] = Var(Seq.empty[String])

  // Entities
  val curEntity         = Var(0L)
  val curEntityLocked   = Var(false)
  var showBackRefs      = false
  var entityLevels      = 1
  var curAttrs          = Seq.empty[String]
  val entityHistorySort = Var("tx")

  // Marker On-values
  var curStars : Set[Long] = Set.empty
  var curFlags : Set[Long] = Set.empty
  var curChecks: Set[Long] = Set.empty

  // Cell id -> marker toggler
  var starTogglers  = Map.empty[String, () => Unit]
  var flagTogglers  = Map.empty[String, () => Unit]
  var checkTogglers = Map.empty[String, () => Unit]

  // Are we toggling multiple rows while holding down toggle key?
  var toggling = false

  // Which kind of marker is being toggled (s/f/c)
  var toggler = ""

  // Undo coordinates
  var curLastTxResults: Array[TxResult]         = Array.empty
  val undone2new      : mutable.Map[Long, Long] = mutable.Map.empty
  val new2undone      : mutable.Map[Long, Long] = mutable.Map.empty

  // Transactions
  val curTxD      : Var[(Long, Long, String)] = Var((0L, 0L, ""))
  val curT        : Var[Long]                 = Var(0L)
  val curTx       : Var[Long]                 = Var(0L)
  val curTxInstant: Var[String]               = Var("")

  // Url
  val curUrl: Var[String] = Var("")

}
