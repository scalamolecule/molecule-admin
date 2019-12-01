package moleculeadmin.client.app.domain
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.common.TopMenu
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.DataTable
import moleculeadmin.client.app.domain.query.{GroupedRender, KeyEvents, QueryBuilder, QuerySubMenu, ViewsRender}
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
object QueryClient extends RxBindings with TreeOps with SchemaOps with ColOps
  with Model2Molecule with AppElements with KeyEvents {

  type keepBooPickleImport_QueryClient = PickleState

  implicit val ctx: Ctx.Owner = rx.Ctx.Owner.safe()

  @JSExport
  def load(db: String): Unit = queryWire().loadMetaData(db).call().map {
    case (dbs, metaSchema, (settings, curViews, stars, flags, checks, queries)) =>

      // Uncomment to test dynamic ScalaFiddle compilation
      //import moleculeadmin.client.app.domain.query.data.groupedit.compileTest.TestScalaFiddle
      //       TestScalaFiddle

      nsMap = mkNsMap(metaSchema)
      viewCellTypes = mkViewCellTypes(nsMap)
      enumAttrs = mkEnumAttrs(nsMap)

      // Set saved settings
      Rx {
        maxRows() = settings.getOrElse("maxRows", "-1").toInt
        limit() = settings.getOrElse("limit", "20").toInt
        builderSelection() = settings.getOrElse("builderSelection", "a")
        savedQueries() = queries
        curStars = stars
        curFlags = flags
        curChecks = checks
        curViews.foreach {
          case "viewsOn"             => viewsOn() = true
          case "viewHelp"            => viewHelp() = true
          case "viewMolecule"        => viewMolecule() = true
          case "viewQueries"         => viewQueries() = true
          case "viewRecentMolecules" => viewRecentMolecules() = true
          case "viewDatalog"         => viewDatalog() = true
          case "viewTransaction"     => viewTransaction() = true
          case "viewEntity"          => viewEntity() = true
          case "viewEntityHistory"   => viewEntityHistory() = true
          case "viewMoleculeModel"   => viewMoleculeModel() = true
          case "viewMoleculeQuery"   => viewMoleculeQuery() = true
          case "viewColumns"         => viewColumns() = true
          case "viewTree1"           => viewTree1() = true
          case "viewTree2"           => viewTree2() = true
          case "showTree3"           => viewTree3() = true
          case _                     =>
        }
      }

      // Render page
      document.body.appendChild(
        TopMenu(dbs, db, "query", QuerySubMenu(db).dynRender).render
      )
      // make saved queries available via key command
      registerKeyEvents
      document.body.appendChild {
        _containerFluid2(
          _row(
            QueryBuilder(db, metaSchema).rxElement,
            DataTable(db).rxElement,
            ViewsRender(db).rxElement,
            GroupedRender(db).rxElement,
          )
        ).render
      }
      _reloadMenuAim()
  }
}
