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

  type keepBooPickleImport = PickleState

  def cb(id: Int, txt: Frag, variable: Var[Boolean]): TypedTag[Div] = Rx(
    _cb(
      "view-" + id,
      txt,
      variable.now,
      { () =>
        variable() = !variable.now
        val openViews = Seq(
          if (showViews.now) Some("showViews") else None,
          if (showMolecule.now) Some("showMolecule") else None,
          if (showQueries.now) Some("showQueries") else None,
          if (showRecentMolecules.now) Some("showRecentMolecules") else None,
          if (showDatalog.now) Some("showDatalog") else None,
          if (showTransaction.now) Some("showTransaction") else None,
          if (showEntity.now) Some("showEntity") else None,
          if (showEntityHistory.now) Some("showEntityHistory") else None,
          if (showMoleculeModel.now) Some("showMoleculeModel") else None,
          if (showMoleculeQuery.now) Some("showMoleculeQuery") else None,
          if (showColumns.now) Some("showColumns") else None,
          if (showTree1.now) Some("showTree1") else None,
          if (showTree2.now) Some("showTree2") else None,
          if (showTree3.now) Some("showTree3") else None
        ).flatten

        val viewsSettings = if (openViews.length == 1) {
          val show = variable.now
          // manually check 'showViews' checkbox on/off
          document.getElementById("checkbox-view-0")
            .asInstanceOf[HTMLInputElement].checked = show
          showViews() = show
          if (show)
            "showViews" +: openViews
          else
            openViews.filterNot(_ == "showViews")
        } else {
          openViews
        }

        // Asynchronously save setting
        queryWire().saveViewSettings(viewsSettings).call().foreach {
          case Left(err) => window.alert(err)
          case Right(_)  =>
            println("Saved open views settings: " +
              viewsSettings.mkString(", "))
        }
      }
    )
  ).now


  def dynRender: Rx.Dynamic[TypedTag[LI]] = Rx {
    _subMenuViews(
      cb(0, span("Show ", span("V", textDecoration.underline), "iews"), showViews),
      hr,
      cb(1, "Molecule", showMolecule),
      cb(2, "Queries", showQueries),
      cb(3, "Recent", showRecentMolecules),
      cb(4, "Datalog", showDatalog),
      cb(5, "Transaction", showTransaction),
      cb(6, "Entity", showEntity),
      cb(7, "Entity History", showEntityHistory),
      h5("Debugging", paddingTop := 10, paddingBottom := 10),
      cb(8, "Molecule Model", showMoleculeModel),
      cb(9, "Molecule Query", showMoleculeQuery),
      cb(10, "Columns", showColumns),
      cb(11, "Tree with attr names only", showTree1),
      cb(12, "Tree with attr definitions", showTree2),
      cb(13, "Full Tree", showTree3)
    )
  }
}
