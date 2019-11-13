package moleculeadmin.client.app.domain.query
import moleculeadmin.shared.ast.query.{Col, Favorite, Filter, QueryCache}
import moleculeadmin.shared.ast.schema.Ns
import moleculeadmin.shared.ast.tree.Tree
import molecule.ast.model.Element
import moleculeadmin.client.app.domain.query.data.RowBuilder
import rx.Var
import scalatags.JsDom.all.s
import scala.collection.mutable.ListBuffer


object QueryState {

  // Schema
  implicit var nsMap: Map[String, Ns] = Map.empty[String, Ns]
  var snippetCellTypes = Map.empty[String, String]
  var enumAttrs        = Seq.empty[String]


  // What is being being processed now? Concatenates part/ns/attr/pos-number as a coordinate in the tree
  val processing = Var[String]("")

  // Query building ---------------------------------

  var newQueryBuildup = true

  // Tree branches
  val tree = Var(Tree(Nil, Nil, Nil, Nil, Nil, Nil))

  // Toggling
  val selection     = Var("a")
  var minimized     = false
  var baseSelection = "a"


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
  val limit       = Var(50)

  var editCellId = ""


  // Caching -----------------------------------

  // Re-use previous query results
  var queryCache = Var(Seq.empty[QueryCache])

  // Avoiding re-creating sort index array during paging
  var cachedCols       = Seq.empty[Col]
  var cachedSortIndex  = Array.empty[Int]
  var cachedRowBuilder = Option.empty[RowBuilder]

  // Avoiding re-creating index array during paging
  var cachedFilters     = Map.empty[Int, Filter[_]]
  var cachedFilterIndex = Array.empty[Int]

  enumAttrs.contains(7)
  cachedFilterIndex.contains(7)

  // Track maxRows change
  var savedMaxRows = -1

  var changeArrays = Map.empty[Int, Array[Int]]


  // Snippets --------------------------------

  val curMolecule = Var("")

  val favorites = Var(Seq.empty[Favorite])

  // Entities
  val curEntity         = Var(0L)
  var curAttrs          = Seq.empty[String]
  val entityHistorySort = Var("tx")
  var curStars          = List.empty[Long]
  var curFlags          = List.empty[Long]
  var curChecks         = List.empty[Long]

  //  curStar.contains(7L)
  //  Array(1) ++= Array(2)

  // Transactions
  val curTxD       = Var((0L, 0L, ""))
  val curT         = Var(0L)
  val curTx        = Var(0L)
  val curTxInstant = Var("")

  // Show snippet statuses
  val showSnippets      = Var(false)
  val showHelp          = Var(false)
  val showMolecule      = Var(false)
  val showFavorites     = Var(false)
  val showCache         = Var(false)
  val showDatalog       = Var(false)
  val showTransaction   = Var(false)
  val showEntity        = Var(false)
  val showEntityHistory = Var(false)

  // Debugging
  val showModel   = Var(false)
  val showQuery   = Var(false)
  val showColumns = Var(false)
  val showTree1   = Var(false)
  val showTree2   = Var(false)
  val showTree3   = Var(false)
}
