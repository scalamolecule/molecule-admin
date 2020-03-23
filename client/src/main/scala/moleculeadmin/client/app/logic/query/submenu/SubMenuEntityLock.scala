package moleculeadmin.client.app.logic.query.submenu

import moleculeadmin.client.app.logic.query.KeyEvents
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.html.query.SubMenuElements
import rx.{Ctx, Rx}
import scalatags.JsDom.all._


case class SubMenuEntityLock()(implicit val ctx: Ctx.Owner)
  extends SubMenuElements with KeyEvents {

  def render: Frag = Rx {
    if (curEntityLocked() && curEntity() != 0) {
      li(
        paddingLeft := 40,
        cursor.pointer,
        i(
          cls := "fas fa-lock",
          color := "#999",
          paddingRight := 5
        ),
        color := "#888",
        curEntity.now,
        onclick := { () => curEntityLocked() = false }
      )
    } else li()
  }
}
