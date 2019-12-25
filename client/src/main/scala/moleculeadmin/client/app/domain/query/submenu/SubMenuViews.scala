package moleculeadmin.client.app.domain.query.submenu
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.html.{Div, LI}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


case class SubMenuViews()(implicit val ctx: Ctx.Owner)
  extends Callbacks with RxBindings with SubMenuElements {

  def cb(n: Int, view: String, label: String): TypedTag[Div] = Rx {
    if (n == 0)
      _cb(
        "view-showViews",
        _cbLabel("␣", label),
        showViews,
        () => toggleShowViews()
      )
    else
      _cb(
        "view-" + view,
        _cbLabel(s"$n", label),
        curViews.now.contains(view),
        () => toggleView(view)
      )
  }.now


  def dynRender: Rx.Dynamic[TypedTag[LI]] = Rx {
    val viewCheckboxes = allViews.zipWithIndex.map {
      case ((view, label), i) => cb(i + 1, view, label)
    }
    _subMenu(
      "submenu-views",
      _shortcut("V", "iews"),
      Seq(
        cb(0, "showViews", "Show Views"),
        hr
      ) ++ viewCheckboxes.take(5)
        ++ Seq(h5("Debugging", paddingTop := 10, paddingBottom := 10))
        ++ viewCheckboxes.drop(5)
    )
  }
}
