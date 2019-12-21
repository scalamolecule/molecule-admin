package moleculeadmin.client.app.domain.query.submenu
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import org.scalajs.dom.html.{Div, LI}
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{document, window}
import rx.{Ctx, Rx, Var}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class Views()(implicit val ctx: Ctx.Owner)
  extends RxBindings with SubMenuElements {

  type keepBooPickleImport_Views = PickleState

  def cb(id: Int, txt: Frag, variable: Var[Boolean]): TypedTag[Div] = Rx(
    _cb(
      "view-" + id,
      txt,
      variable.now,
      { () =>
        variable() = !variable.now
        val openViews = Seq(
          if (viewsOn.now) Some("viewsOn") else None,
          if (viewMolecule.now) Some("viewMolecule") else None,
          if (viewDatalog.now) Some("viewDatalog") else None,
          if (viewTransaction.now) Some("viewTransaction") else None,
          if (viewEntity.now) Some("viewEntity") else None,
          if (viewEntityHistory.now) Some("viewEntityHistory") else None,
          if (viewMoleculeModel.now) Some("viewMoleculeModel") else None,
          if (viewMoleculeQuery.now) Some("viewMoleculeQuery") else None,
          if (viewColumns.now) Some("viewColumns") else None,
          if (viewTree1.now) Some("viewTree1") else None,
          if (viewTree2.now) Some("viewTree2") else None,
          if (viewTree3.now) Some("viewTree3") else None
        ).flatten

        val viewsSettings = if (openViews.length == 1) {
          val show = variable.now
          // manually check 'showViews' checkbox on/off
          document.getElementById("checkbox-view-0")
            .asInstanceOf[HTMLInputElement].checked = show
          viewsOn() = show
          if (show)
            "viewsOn" +: openViews
          else
            openViews.filterNot(_ == "viewsOn")
        } else {
          openViews
        }

        // Asynchronously save setting
        queryWire().saveOpenViews(viewsSettings).call().foreach {
          case Left(err) => window.alert(err)
          case Right(_)  =>
            println("Saved open views settings: " +
              viewsSettings.mkString(", "))
        }
      }
    )
  ).now


  def dynRender: Rx.Dynamic[TypedTag[LI]] = Rx {
    _subMenu(
      "Views",
      Seq(
        cb(0, span("Show ", span("V", textDecoration.underline), "iews"), viewsOn),
        hr,
        cb(1, "Molecule", viewMolecule),
        cb(2, "Datalog", viewDatalog),
        cb(3, "Transaction", viewTransaction),
        cb(4, "Entity", viewEntity),
        cb(5, "Entity History", viewEntityHistory),
        h5("Debugging", paddingTop := 10, paddingBottom := 10),
        cb(6, "Molecule Model", viewMoleculeModel),
        cb(7, "Molecule Query", viewMoleculeQuery),
        cb(8, "Columns", viewColumns),
        cb(9, "Tree with attr names only", viewTree1),
        cb(10, "Tree with attr definitions", viewTree2),
        cb(11, "Full Tree", viewTree3)
      )
    )
  }
}
