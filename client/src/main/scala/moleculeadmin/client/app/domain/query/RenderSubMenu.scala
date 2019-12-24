package moleculeadmin.client.app.domain.query
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.submenu.{SubMenuQueryList, _}
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.html.{LI, Span}
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class RenderSubMenu(db: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with SubMenuElements {

  type keepBooPickleImport_QuerySubMenu = PickleState

  _maxRowsSelector.onchange = _ => {
    maxRows() = _maxRowsSelector.value.toInt

    // Asynchronously save setting
    queryWire().saveSetting("maxRows", maxRows.now.toString).call().foreach {
      case Left(err) => window.alert(err)
      case Right(_)  => println("Saved setting for `maxRows`: " + maxRows.now)
    }

    _maxRowsSelector.blur()
  }


  def dynRender: Rx.Dynamic[JsDom.TypedTag[Span]] = Rx {
//    println("---- submenu")
    // Re-calc sub menu when query changes
    curMolecule()

    span(
      _maxRowsSelector,
      ul(cls := "nav nav-pills",
        if (
          savedQueries.nonEmpty
            || recentQueries.nonEmpty
            || modelElements.now.nonEmpty
        ) SubMenuQueryList(db).dynRender else (),

        if (modelElements.now.nonEmpty) {
          Seq(
            SubMenuGrouped().dynRender,
            SubMenuViews().dynRender,
          )
        } else (),

        SubMenuShortCuts().dynRender
      )
    )
  }
}
