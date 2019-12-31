package moleculeadmin.client.app.domain
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.common.TopMenu
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.{RenderGrouped, RenderQueryBuilder, RenderSubMenu, RenderViews, _}
import moleculeadmin.client.app.domain.query.data.DataTable
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.query.{ColOps, SchemaOps}
import moleculeadmin.shared.ops.transform.Model2Molecule
import org.scalajs.dom.document
import rx.{Ctx, Rx}
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


@JSExportTopLevel("QueryClient")
object QueryClient
  extends RxBindings with TreeOps with SchemaOps with ColOps
  with Model2Molecule with AppElements with KeyEvents {

  type keepBooPickleImport_QueryClient = PickleState

  // Single Rx context to be passed around wherever needed
  implicit val ctx: Ctx.Owner = rx.Ctx.Owner.safe()

  @JSExport
  def load(db0: String): Unit = queryWire().loadMetaData(db0).call().map {
    case (dbs, metaSchema, (settings, stars, flags, checks, queries)) =>

      // Uncomment to test dynamic ScalaFiddle compilation
      // import moleculeadmin.client.app.domain.query.data.groupedit.compileTest.TestScalaFiddle
      // TestScalaFiddle

      // Base settings
      db = db0
      nsMap = mkNsMap(metaSchema)
      viewCellTypes = mkViewCellTypes(nsMap)
      enumAttrs = mkEnumAttrs(nsMap)

      //      settings.toList.sortBy(_._1) foreach println
      //      println("---------")
      //      queries foreach println
      //      println("---------")

      // Saved custom settings
      Rx {
        maxRows() = settings.getOrElse("maxRows", "-1").toInt
        limit() = settings.getOrElse("limit", "20").toInt
        querySelection() = settings.getOrElse("querySelection", "a")
        showViews = settings.getOrElse("showViews", "false").toBoolean
        curViews() = settings.collect {
          case (k, "true") if k.startsWith("view") => k
        }.toSeq
        savedQueries = queries
        curStars = stars
        curFlags = flags
        curChecks = checks
      }

      // Render page
      document.body.appendChild(
        TopMenu(dbs, db, "query", RenderSubMenu().dynRender).render
      )
      // make saved queries available via key command
      registerKeyEvents
      document.body.appendChild {
        _containerFluid2(
          _row(
            RenderQueryBuilder(metaSchema).dynRender,
            DataTable().dynRender,
            RenderGrouped().dynRender,
            RenderViews().dynRender,
          )
        ).render
      }
      _reloadMenuAim()
  }
}
