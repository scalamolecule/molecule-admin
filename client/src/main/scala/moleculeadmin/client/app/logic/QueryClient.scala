package moleculeadmin.client.app.logic

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.app.logic.common.TopMenu
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query._
import moleculeadmin.client.app.logic.query.data.DataTable
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.query.{ColOps, SchemaOps}
import moleculeadmin.shared.ops.transform.Model2Molecule
import org.scalajs.dom.{document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.all._
import util.client.rx.RxBindings
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


@JSExportTopLevel("QueryClient")
object QueryClient
  extends RxBindings with TreeOps with SchemaOps with ColOps
    with Model2Molecule with AppElements with KeyEvents with UrlHandling {

  // Single Rx context to be passed around wherever needed
  implicit val ctx: Ctx.Owner = rx.Ctx.Owner.safe()


  @JSExport
  def load(db0: String): Unit = queryWireAjax().loadMetaData(db0).call().foreach {
    case Right(pageMetaData) =>
      val dbs = init(db0, pageMetaData)
      document.body.appendChild(
        TopMenu(dbs, db, "query", RenderSubMenu().dynRender).render
      )
      document.body.appendChild {
        _containerFluid2(
          _row(
            RenderQueryBuilder(metaSchema).dynRender,
            DataTable().dynRender,
            RenderUndo().dynRender,
            RenderGrouped().dynRender,
            RenderViews().dynRender,
          )
        ).render
      }
      post()

    case Left(err) =>
      window.alert(s"Error loading Query page:\n$err")
  }


  def init(db0: String, pageMetaData: PageMetaData): Seq[String] = {
    val (
      dbs, metaSchema0,
      (settings, stars, flags, checks, undoneTs, queries, editExprs0)
      ) = pageMetaData

    prepareBrowserHistory

    // Uncomment to test dynamic ScalaFiddle compilation
    // import moleculeadmin.client.app.domain.query.data.groupedit.compileTest.TestScalaFiddle
    // TestScalaFiddle

    // Base settings
    db = db0
    metaSchema = metaSchema0
    val dbMetaInfo = mkNsMap(metaSchema)
    nsMap = dbMetaInfo._1
    valuesCounted = dbMetaInfo._2
    viewCellTypes = mkViewCellTypes(nsMap)
    enumAttrs = mkEnumAttrs(nsMap)

    // Saved custom settings
    Rx {
      maxRows() = settings.getOrElse("maxRows", "-1").toInt
      limit() = settings.getOrElse("limit", "20").toInt
      gridType() = settings.getOrElse("gridType", "1").toInt
      querySelection() = settings.getOrElse("querySelection", "a")
      if (valuesCounted) {
        queryBaseSelection = settings.getOrElse("queryBaseSelection", "a")
      } else {
        queryBaseSelection = "a"
      }
      showViews = settings.getOrElse("showViews", "false").toBoolean
      showBackRefs = settings.getOrElse("showBackRefs", "false").toBoolean
      entityLevels = settings.getOrElse("entityLevels", "1").toInt
      entityHistorySort() = settings.getOrElse("entityHistorySort", "tx")

      curViews() = settings.collect {
        case (k, "true") if k.startsWith("view") => k
      }.toSeq

      // Bit-decode newT/undoneT pairs
      var newT    = 0L
      var undoneT = 0L
      undoneTs.foreach { pair =>
        newT = pair >> 32
        undoneT = pair & 0xFFFFFFF
        new2undone += newT -> undoneT
        undone2new += undoneT -> newT
      }

      savedQueries = queries
      editExprs ++= editExprs0
      curStars = stars
      curFlags = flags
      curChecks = checks

      loadOptionalMoleculeFromUrl
    }
    dbs
  }

  def post(): Unit = {
    // make saved queries available via key command
    registerKeyEvents
    // make dropdowns forgiving
    _reloadMenuAim()
  }
}
