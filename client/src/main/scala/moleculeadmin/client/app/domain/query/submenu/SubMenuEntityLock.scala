package moleculeadmin.client.app.domain.query.submenu

import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.SubMenuElements
import rx.{Ctx, Rx}
import scalatags.JsDom.all._


case class SubMenuEntityLock()(implicit val ctx: Ctx.Owner)
  extends SubMenuElements with KeyEvents {

  def render: Frag = Rx {
    if (curEntityLocked() && curEntity() != 0) {
      li(
        paddingLeft := 40,
        color := "#888",
        i(
          cls := "fas fa-lock",
          color := "#aaa",
          paddingRight := 5
        ),
        curEntity.now
      )
    } else li()
  }
}
