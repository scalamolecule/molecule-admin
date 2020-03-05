package moleculeadmin.client.app.domain.query.submenu

import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.{Callbacks, KeyEvents}
import moleculeadmin.client.app.element.query.SubMenuElements
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
