package moleculeadmin.client.app.logic

import autowire._
import boopickle.Default._
import util.client.rx.RxBindings
import moleculeadmin.client.app.logic.common.TopMenu
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.DataTable
import moleculeadmin.client.app.logic.query.{RenderGrouped, RenderQueryBuilder, RenderSubMenu, RenderViews, _}
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.query.{ColOps, SchemaOps}
import moleculeadmin.shared.ops.transform.Model2Molecule
import org.scalajs.dom.raw.MessageEvent
import org.scalajs.dom.{WebSocket, document}
import rx.{Ctx, Rx}
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


@JSExportTopLevel("QueryClient")
object QueryClient
  extends RxBindings with TreeOps with SchemaOps with ColOps
    with Model2Molecule with AppElements with KeyEvents with UrlHandling {

  // Single Rx context to be passed around wherever needed
  implicit val ctx: Ctx.Owner = rx.Ctx.Owner.safe()

  prepareBrowserHistory

  @JSExport
  def load(db0: String): Unit = queryWireAjax().loadMetaData(db0).call().map {
    case (dbs, metaSchema0, (settings, stars, flags, checks, undoneTs, queries)) =>

      // Uncomment to test dynamic ScalaFiddle compilation
      // import moleculeadmin.client.app.domain.query.data.groupedit.compileTest.TestScalaFiddle
      // TestScalaFiddle

      //      val socket = QueryWebSocket().socket
      //      socket.onmessage = { e: MessageEvent =>
      //        println("onmessage: " + e.data)
      //      }
      //      socket.send("sent from client")


      // Base settings
      db = db0
      metaSchema = metaSchema0
      val dbMetaInfo = mkNsMap(metaSchema)
      nsMap = dbMetaInfo._1
      valuesCounted = dbMetaInfo._2
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
        if (valuesCounted) {
          queryBaseSelection = settings.getOrElse("queryBaseSelection", "a")
        } else {
          queryBaseSelection = "a"
        }
        showViews = settings.getOrElse("showViews", "false").toBoolean
        showEntityViewBackRefs =
          settings.getOrElse("showEntityViewBackRefs", "false").toBoolean
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
        curStars = stars
        curFlags = flags
        curChecks = checks

        loadOptionalMolecule
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
            RenderUndo().dynRender,
            RenderGrouped().dynRender,
            RenderViews().dynRender,
          )
        ).render
      }
      _reloadMenuAim()
  }
}
