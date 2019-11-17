package moleculeadmin.client.app.domain.query
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.submenu._
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.html.Span
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class QuerySubMenu(db: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with SubMenuElements {

  type keepBooPickleImport = PickleState

  _maxRowsSelector.onchange = _ => {
    maxRows() = _maxRowsSelector.value.toInt

    // Asynchronously save setting
    queryWire().saveSetting("maxRows", maxRows.now.toString).call().foreach {
      case Left(err) => window.alert(err)
      case Right(_)  => println("Saved max rows setting: " + maxRows.now)
    }

    _maxRowsSelector.blur()
  }

  def dynRender: Rx.Dynamic[JsDom.TypedTag[Span]] = Rx {
    // Update whenever cache updates
    queryCache()
    span(
      _maxRowsSelector,
      ul(cls := "nav nav-pills",
        Queries(db).dynRender,
        if (modelElements().nonEmpty) {
          Seq(
            RecentMolecules(db).dynRender,
            Views().dynRender,
            ShortCuts().dynRender
          )
        } else ()
      )
    )
  }
}
