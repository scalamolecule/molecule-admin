package moleculeadmin.client.app.logic.query.submenu

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.{Callbacks, KeyEvents}
import moleculeadmin.client.app.html.query.SubMenuElements
import org.scalajs.dom.html.LI
import rx.Ctx
import scalatags.JsDom.all._


case class SubMenuUndo()(implicit val ctx: Ctx.Owner)
  extends Callbacks with KeyEvents {


  def render: LI =
    li(
      paddingLeft := 12,
      cursor.pointer,
      _shortcut("U", "ndo"),
      onclick := { () =>
        toggleUndo()
      }
    ).render
}
