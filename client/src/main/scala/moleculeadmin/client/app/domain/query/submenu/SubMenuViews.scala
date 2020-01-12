package moleculeadmin.client.app.domain.query.submenu
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.SubMenuElements
import org.scalajs.dom.html.LI
import rx.Ctx
import scalatags.JsDom.all._


case class SubMenuViews()(implicit val ctx: Ctx.Owner)
  extends Callbacks with SubMenuElements {


  def render: LI = {
    val viewCheckboxes = allViews.zipWithIndex.map {
      case ((view, label), i) =>
        _cb(
          "view-" + view,
          _cbLabel(s"${i + 1}", label),
          curViews.now.contains(view),
          () => toggleView(view)
        )
    }

    _subMenu(
      "submenu-views",
      _shortcut("V", "iews"),
      Seq(
        _cb(
          "view-showViews",
          _cbLabel("␣", "Show Views"),
          showViews,
          () => toggleShowViews()
        ),
        hr
      ) ++ viewCheckboxes.take(5)
        ++ Seq(h5("Debugging", paddingTop := 10, paddingBottom := 10))
        ++ viewCheckboxes.drop(5)
    ).render
  }
}