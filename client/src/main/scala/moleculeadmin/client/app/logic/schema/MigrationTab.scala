package moleculeadmin.client.app.logic.schema
import boopickle.Default._
import util.client.rx.RxBindings
import moleculeadmin.shared.api.BaseApi
import moleculeadmin.shared.ast.metaSchema.FlatSchema
import org.scalajs.dom.html.Div
import rx.{Ctx, Rx, Var}
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


@JSExportTopLevel("MigrationTab")
object MigrationTab extends BaseApi with RxBindings {

  implicit val ctx: Ctx.Owner = rx.Ctx.Owner.safe()

  val syncing = Var[Boolean](false)


  @JSExport
  def load(db: String, metaSchema: FlatSchema): JsDom.TypedTag[Div] = {
//    import rx.Ctx.Owner.Unsafe._


    div(
      "Migration",
      p(),
      Rx(
        a("link1",
          href := "#",
          onclick := { () =>
            syncing() = !syncing.now
          }
        )
      ),
      p(),
      Rx(
        div(
          a("link2",
            href := "#",
            onclick := { () =>
              syncing() = !syncing.now
            }
          ),

          if (syncing()) {
            div("true")
          } else {
            div("false")
          }
        )
      )
    )
  }
}
