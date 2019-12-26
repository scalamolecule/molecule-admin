package moleculeadmin.client.app.domain.query.data
import autowire._
import boopickle.Default._
import molecule.ast.model.{Atom, Model}
import molecule.ast.query.QueryExpr
import molecule.ops.VerifyRawModel
import molecule.transform.{Model2Query, Query2String}
import moleculeadmin.client.app.domain.QueryClient._
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.datatable.TableElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.Filter
import moleculeadmin.shared.ops.query.ModelOps
import org.scalajs.dom.document
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.HTMLElement
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class DataTable()(implicit val ctx: Ctx.Owner)
  extends RxBindings with KeyEvents with ModelOps with TableElements {

  type keepBooPickleImport_DataTable = PickleState

  // Table elements
  val tableHead: TableSection = thead().render
  val tableBody: TableSection = tbody(id := "tableBody",
    tr(td(), td(colspan := columns.now.size, "fetching data..."))
  ).render
  val tableFoot: TableSection = tfoot().render

  val tableContainer = _dataTableContainer(
    _dataTable(
      tableHead,
      tableBody,
      tableFoot
    )
  )

  def fetchAndPopulate(
    tableBody: TableSection,
    tableFoot: TableSection
  ): Unit = {
    val (query, _)   = Model2Query(Model(modelElements.now))
    val datalogQuery = molecule.transform.Query2String(query).toMap
    val resolve      = (expr: QueryExpr) => Query2String(query).p(expr)
    val rules        = if (query.i.rules.nonEmpty)
      Some("[" + (query.i.rules map resolve mkString " ") + "]") else None
    val (l, ll, lll) = encodeInputs(query)

    // Fetch data from db asynchronously
    queryWire()
      .query(db, datalogQuery, rules, l, ll, lll, maxRows.now, columns.now)
      .call().foreach {
      case Right(queryResult) =>
        //        println("QUERY")
        rowCountAll = queryResult.rowCountAll
        rowCount = queryResult.rowCount
        DataTableBodyFoot().populate(tableBody, tableFoot, queryResult)

      case Left(Nil) =>
        //        println("Empty result")
        tableBody.innerHTML = ""
        tableBody.appendChild(
          tr(td(colspan := columns.now.size + 1, "Empty result set...")).render
        )
        tableFoot.innerHTML = ""
        rowCountAll = 0
        renderSubMenu.recalc()


      case Left(msgs) =>
        val dataTableContainer = document.getElementById("dataTableContainer")
        dataTableContainer.innerHTML = ""
        dataTableContainer.appendChild(
          div(
            p(), p(b("An error occurred when querying")), pre(msgs.head),
            p(), p(b("Model")), pre(modelElements.now.mkString("\n")),
            p(), p(b("Query")), pre(query.toString()),
            p(), p(b("Datalog")), pre(datalogQuery),
            p(), p(b("Cols")), pre(columns.now.mkString("\n")),
            p(), p(b("Stacktrace")), ul(for (l <- msgs.tail) yield li(l))
          ).render
        )
    }
  }

  def dynRender: Rx.Dynamic[JsDom.TypedTag[HTMLElement]] = Rx {
    //    println("---- table")

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
      modelElements() match {
        case Nil => span()

        case elements if emptyNamespaces(elements).nonEmpty => {
          tree() = mkTree(mkModelTree(elements))
          columns() = getCols(elements)
          _rowCol1(
            "Please select non-generic attr/ref in empty namespaces:",
            ul(for (ns <- emptyNamespaces(elements)) yield li(ns))
          )
        }

        case elements if nsHasOnlyDummyAttr(elements) => {
          tree() = mkTree(mkModelTree(elements))
          columns() = getCols(elements)
          _rowCol1(
            "Please add non-generic attr/ref too."
          )
        }

        case Atom(_, attr, _, _, _, _, _, _) :: Nil
          if attr.last == '_' || attr.last == '$' =>
          _rowCol1("Please select at least one mandatory attribute")

        case elements =>
          //          println("----------- new model ----------")
          val elements1 = VerifyRawModel(elements)
          tree() = mkTree(mkModelTree(elements1))
          curMolecule() = model2molecule(elements1)
          columns() = getCols(elements)
          eidCols = getEidTableColIndexes(columns.now)
          filters() = Map.empty[Int, Filter[_]]
          offset() = 0
          curAttrs = columns.now.map(c => s":${c.nsFull}/${c.attr}")

          groupableCols = getGroupableCols(columns.now)

          // trigger views (columns trigger killed, so that is dormant here)
          curViews.recalc()

          registerKeyEvents
          DataTableHead(tableBody).populate(tableHead)
          fetchAndPopulate(tableBody, tableFoot)
          tableContainer
      }
    } catch {
      case e: Throwable => {
        _rowCol2(
          p(), p("Unexpected table render error:"),
          pre(e.getMessage),
          p(), p(b("Model")), pre(modelElements.now.mkString("\n")),
          p(), p(b("Cols")), pre(columns.now.mkString("\n")),
          p(), p(b("Stacktrace")), ul(
            for (l <- e.getStackTrace) yield li(l.toString)
          )
        )
      }
    }
  }
}

