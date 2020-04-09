package moleculeadmin.client.app.logic.query.data

import autowire._
import boopickle.Default._
import molecule.ast.model._
import molecule.ast.query.QueryExpr
import molecule.ops.VerifyRawModel
import molecule.transform.{Model2Query, Query2String}
import moleculeadmin.client.app.html.query.datatable.TableElements
import moleculeadmin.client.app.logic.QueryClient._
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.{Callbacks, KeyEvents, UrlHandling}
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.{Col, Filter, QueryResult}
import moleculeadmin.shared.ops.query.ModelOps
import org.scalajs.dom.document
import org.scalajs.dom.html.{Div, TableSection}
import org.scalajs.dom.raw.{HTMLElement, Node}
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class DataTable()(implicit val ctx: Ctx.Owner)
  extends Callbacks
    with KeyEvents with ModelOps with TableElements with UrlHandling {


  def dynRender: Rx.Dynamic[JsDom.TypedTag[HTMLElement]] = Rx {
    // Var's not to trigger this Rx
    // (`modelElements` and `maxRows` are the main triggers)
    columns.kill()
    filters.kill()
    offset.kill()
    limit.kill()

    // de-select markers
    curEntity() = 0
    curT() = 0L
    curTx() = 0L
    curTxInstant() = ""

    // Re-calculate when max rows changes
    maxRows()

    try {
      // modelElements is the main trigger
      // Whenever the model changes, re-render
      modelElements() match {
        case Nil =>
          if (querySelection().isEmpty)
            _buildQueryClosed("Open query builder by pressing ´q´")
          else
            span()

        case elements if emptyNamespaces(elements).nonEmpty => {
          curMolecule() = ""
          reset()
          _rowCol1(
            "Please add non-generic attr/ref in empty namespaces:",
            ul(emptyNamespaces(elements).map(li(_)))
          )
        }

        case elements if hasIncompleteBranches(elements) => {
          curMolecule() = model2molecule(modelElements.now)
          reset()
          _rowCol1("Please add at least one mandatory attribute")
        }

        case elements =>
          renderDataTable(elements)
      }
    } catch {
      case e: Throwable => {
        _rowCol2(
          p(), p("Unexpected table render error:"),
          pre(e.getMessage),
          p(), p(b("Model")), pre(modelElements.now.mkString("\n")),
          p(), p(b("Cols")), pre(columns.now.mkString("\n")),
          p(), p(b("Stacktrace")), ul(
            for (l <- e.getStackTrace.toSeq) yield li(l.toString)
          )
        )
      }
    }
  }


  def reset(): Unit = {
    tree() = mkTree(mkModelTree(modelElements.now))
    columns() = getCols(modelElements.now)
    curEntity() = 0L
    pushUrlOntoHistoryStack = true
    pushUrl()
    curViews.recalc()
  }


  def renderDataTable(elements: Seq[Element]): JsDom.TypedTag[Div] = {
    val elements1 = VerifyRawModel(elements)
    tree() = mkTree(mkModelTree(elements1))
    curMolecule() = model2molecule(elements1)
    curEntityLocked() = false
    columns() = getCols(elements1)

    // See if we have a matching sorting from before
    recentQueries.find(_.molecule == curMolecule.now) match {
      case Some(q) => setColumns(q)
      case None    => savedQueries.find(_.molecule == curMolecule.now).foreach { q =>
        setColumns(q)
      }
    }

    eidCols = getEidTableColIndexes(columns.now)
    filters() = Map.empty[Int, Filter[_]]
    offset() = 0
    curAttrs = columns.now.map(c => s":${c.nsFull}/${c.attr}")

    // Re-calculate other elements
    showUndo.recalc()
    groupedColIndexes.recalc()
    curViews.recalc()

    registerKeyEvents
    DataTableHead(tableBody).populate(tableHead)
    fetchAndPopulate(tableBody, tableFoot)
    tableContainer
  }


  def fetchAndPopulate(
    tableBody: TableSection,
    tableFoot: TableSection
  ): Unit = {
    //    println("fetchAndPopulate...")
    val (query, _, _, _) = Model2Query(Model(modelElements.now))
    val datalogQuery     = molecule.transform.Query2String(query).multiLine(60)
    val resolve          = (expr: QueryExpr) => Query2String(query).p(expr)
    val rules            = if (query.i.rules.nonEmpty)
      Some("[" + (query.i.rules map resolve mkString " ") + "]") else None
    val (l, ll, lll)     = encodeInputs(query)
    groupableCols = getGroupableCols(columns.now)

    // Temporary showing
    resetTableBodyFoot("Fetching data...")

    // Push url with new query onto browser history
    pushUrl()

    // Fetch data from db asynchronously
    queryWireAjax()
      .query(db, datalogQuery, rules, l, ll, lll, maxRows.now, columns.now)
      .call().foreach {

      case Right(queryResult) =>
        rowCountAll = queryResult.rowCountAll
        rowCount = queryResult.rowCount

        // Render data table
        DataTableBodyFoot().populate(tableBody, tableFoot, queryResult)

        // Render grouped
        savedQueries.find(_.molecule == curMolecule.now) match {
          case None        =>
            showGrouped = false
            groupedColIndexes() = Set.empty[Int]
          case Some(query) =>
            showGrouped = query.showGrouped
            if (groupedColIndexes.now == query.groupedColIndexes)
              groupedColIndexes.recalc()
            else
              groupedColIndexes() = query.groupedColIndexes
        }
        renderSubMenu.recalc()

        recentQueries =
          curQuery +: recentQueries.filterNot(_.molecule == curMolecule.now)

      case Left(Nil) =>
        resetTableBodyFoot("Empty result set", false)
        rowCountAll = 0
        renderSubMenu.recalc()

      case Left(msgs) =>
        val datalogQuery       = molecule.transform.Query2String(query).multiLine()
        val dataTableContainer = document.getElementById("dataTableContainer")
        dataTableContainer.innerHTML = ""
        dataTableContainer.appendChild(
          div(
            p(), p(b("An error occurred when querying")), pre(msgs.head),
            p(), p(b("Model")), pre(modelElements.now.mkString("\n")),
            p(), p(b("Query")), pre(query.toString()),
            p(), p(b("Datalog")), pre(datalogQuery),
            p(), p(b("Cols")), pre(columns.now.mkString("\n")),
            p(), p(b("Stacktrace")), ul(msgs.tail.map(li(_)))
          ).render
        )
    }
  }

  // Render elements -----------------------------------------

  lazy val tableContainer          = _dataTableContainer(
    _dataTable(
      tableHead,
      tableBody,
      tableFoot
    )
  )
  lazy val tableHead: TableSection = thead().render
  lazy val tableBody: TableSection = tbody(id := "tableBody",
    tr(td(), td(colspan := columns.now.size, "fetching data..."))
  ).render
  lazy val tableFoot: TableSection = tfoot().render

  def resetTableBodyFoot(msg: String, showSpinner: Boolean = true): Node = {
    tableBody.innerHTML = ""
    tableFoot.innerHTML = ""
    tableFoot.appendChild(
      tr(
        td(
          colspan := columns.now.size + 1,
          if(showSpinner) _sync(0, 6) else (),
          msg
        )
      ).render
    )
  }

}

