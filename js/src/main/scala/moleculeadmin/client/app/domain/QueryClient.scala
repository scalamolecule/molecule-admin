package moleculeadmin.client.app.domain
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.app.domain.common.TopMenu
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.DataTable
import moleculeadmin.client.app.domain.query.{KeyEvents, QueryBuilder, QuerySubMenu, SnippetRender}
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

  type keepBooPickleImport = PickleState

  implicit val ctx: Ctx.Owner = rx.Ctx.Owner.safe()


  @JSExport
  def load(db: String): Unit = queryWire().loadMetaData(db).call().map {
    case (dbs, metaSchema, settings) =>

      nsMap = mkNsMap(metaSchema)
      snippetCellTypes = mkSnippetCellTypes(nsMap)
      enumAttrs = mkEnumAttrs(nsMap)

      Rx {
        // Set open snippets
        settings._1.foreach {
          case "showSnippets"      => showSnippets() = true
          case "showMolecule"      => showMolecule() = true
          case "showFavorites"     => showFavorites() = true
          case "showCache"         => showCache() = true
          case "showDatalog"       => showDatalog() = true
          case "showTransaction"   => showTransaction() = true
          case "showEntity"        => showEntity() = true
          case "showEntityHistory" => showEntityHistory() = true
          case "showModel"         => showModel() = true
          case "showQuery"         => showQuery() = true
          case "showColumns"       => showColumns() = true
          case "showTree1"         => showTree1() = true
          case "showTree2"         => showTree2() = true
          case "showTree3"         => showTree3() = true
          case _                   =>
        }

        // Load favorites
        favorites.now match {
          case Nil => favorites() = settings._2
          case _   =>
        }
      }

      // Render page
      document.body.appendChild(
        TopMenu(dbs, db, "query", QuerySubMenu(db).dynRender).render
      )
      // make favorites available via key command
      registerKeyEvents
      document.body.appendChild {
        //        println("append all...")
        _containerFluid2(
          _row(
            QueryBuilder(db, metaSchema).rxElement,
            DataTable(db).rxElement,
            SnippetRender(db).rxElement
          )
        ).render
      }
      _reloadMenuAim()
  }
}
