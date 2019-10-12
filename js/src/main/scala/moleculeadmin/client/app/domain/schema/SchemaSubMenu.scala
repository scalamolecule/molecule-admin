package moleculeadmin.client.app.domain.schema
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.SchemaClient.tab
import moleculeadmin.client.rxstuff.RxBindings
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
