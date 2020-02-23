package moleculeadmin.client.app.domain.query

import moleculeadmin.shared.ast.query.{Col, Filter, QueryCache, QueryDTO}
import moleculeadmin.shared.ast.schema.Ns
import moleculeadmin.shared.ast.tree.Tree
import molecule.ast.model.Element
import moleculeadmin.client.app.domain.query.data.RowBuilder
import moleculeadmin.shared.api.QueryApi
import rx.Var
import scalatags.JsDom.all.s
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object QueryState extends QueryApi {

  var db = ""

  // Db settings id so that we can avoid looking it up on each marker toggle
  var dbSettingsIdOpt = Option.empty[Long]

  // Schema ----------------------------------------
  implicit var nsMap: Map[String, Ns] = Map.empty[String, Ns]
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
    ("view06_MoleculeModel", "Molecule Model"),
    ("view07_MoleculeQuery", "Molecule Query"),
    ("view08_Columns", "Columns"),
    ("view09_Tree1", "Tree with attr names only"),
    ("view10_Tree2", "Tree with attr definitions"),
    ("view11_Tree3", "Full Tree")
  )
  val curViews: Var[Seq[String]] = Var(Seq.empty[String])


  // Entities
  val curEntity        : Var[Long]   = Var(0L)
  var curEntityLocked  : Boolean     = false
  var curAttrs         : Seq[String] = Seq.empty
  val entityHistorySort: Var[String] = Var("tx")

  // tableCol = colIndex + 1
  // tableCol -> (eid -> array indexes)
  val curEntityIndexes: mutable.Map[Int, mutable.LongMap[List[Int]]] =
    mutable.Map.empty

  // For fast render on page scrolling
  // tableCol -> array of marker status
  val curStarIndexes : mutable.Map[Int, Array[Boolean]] = mutable.Map.empty
  val curFlagIndexes : mutable.Map[Int, Array[Boolean]] = mutable.Map.empty
  val curCheckIndexes: mutable.Map[Int, Array[Boolean]] = mutable.Map.empty

  // Marker On-values
  var curStars : Set[Long] = Set.empty
  var curFlags : Set[Long] = Set.empty
  var curChecks: Set[Long] = Set.empty

  // Undo coordinates
  var curLastTxs: Array[TxData]           = Array.empty
  val undone2new: mutable.Map[Long, Long] = mutable.Map.empty
  val new2undone: mutable.Map[Long, Long] = mutable.Map.empty

  // Transactions
  val curTxD      : Var[(Long, Long, String)] = Var((0L, 0L, ""))
  val curT        : Var[Long]                 = Var(0L)
  val curTx       : Var[Long]                 = Var(0L)
  val curTxInstant: Var[String]               = Var("")
}
