package moleculeadmin.client.app.logic.schema
import util.client.rx.RxBindings
import moleculeadmin.client.app.html.query.SubMenuElements
import moleculeadmin.client.app.logic.SchemaClient.tab
import rx.{Ctx, Rx}
import scalatags.JsDom.all._


case class SchemaSubMenu()(implicit val ctx: Ctx.Owner) extends RxBindings with SubMenuElements {

  def render = Rx {
    span(
      ul(cls := "nav nav-pills",
        tab(1, "Define", true),
        tab(2, "Value"),
        tab(3, "Sync")
      )
    )
  }
}
