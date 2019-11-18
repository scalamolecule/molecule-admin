package moleculeadmin.client.app.domain
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.element.schema.SubMenuElements
import moleculeadmin.client.app.domain.common.TopMenu
import moleculeadmin.client.app.domain.schema.{DefineTab, SyncTab, ValueTab}
import moleculeadmin.client.autowire.schemaWire
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.document
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("SchemaClient")
object SchemaClient extends RxBindings with SubMenuElements {
  type keepBooPickleImport = PickleState

  implicit val ctx = rx.Ctx.Owner.safe()

  def subMenu = span(
    ul(cls := "nav nav-pills",
      tab(1, "Define", true),
      tab(2, "Value"),
      tab(3, "Sync")
    )
  )

  @JSExport
  def load(db: String): Unit = schemaWire().getSchemas2(db).call().foreach {
    case (dbs, flatAttrs, metaSchema) =>

      document.body.appendChild(TopMenu(dbs, db, "schema", subMenu).render)
      document.body.appendChild(
        _containerFluid2(
          _row(
            tabContent(
              tabPane(1, true)(DefineTab(db, metaSchema).render2),
              tabPane(2)(ValueTab(db, flatAttrs).render2),
              tabPane(3)(SyncTab(db).render)
            )
          )
        ).render
      )
  }

}
