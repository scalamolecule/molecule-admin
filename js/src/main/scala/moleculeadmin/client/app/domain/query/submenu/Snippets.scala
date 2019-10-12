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


case class Snippets()(implicit val ctx: Ctx.Owner)
  extends RxBindings with SubMenuElements {

  type keepBooPickleImport = PickleState

  def cb(id: Int, txt: Frag, variable: Var[Boolean]): TypedTag[Div] = Rx(
    _cb(
      "snippet-" + id,
      txt,
      variable.now,
      { () =>
        variable() = !variable.now
        val openSnippets = Seq(
          if (showSnippets.now) Some("showSnippets") else None,
          if (showMolecule.now) Some("showMolecule") else None,
          if (showFavorites.now) Some("showFavorites") else None,
          if (showCache.now) Some("showCache") else None,
          if (showDatalog.now) Some("showDatalog") else None,
          if (showTransaction.now) Some("showTransaction") else None,
          if (showEntity.now) Some("showEntity") else None,
          if (showEntityHistory.now) Some("showEntityHistory") else None,
          if (showModel.now) Some("showModel") else None,
          if (showQuery.now) Some("showQuery") else None,
          if (showColumns.now) Some("showColumns") else None,
          if (showTree1.now) Some("showTree1") else None,
          if (showTree2.now) Some("showTree2") else None,
          if (showTree3.now) Some("showTree3") else None
        ).flatten

        val snippetSettings = if (openSnippets.length == 1) {
          val show = variable.now
          // manually check 'showSnippets' checkbox on/off
          document.getElementById("checkbox-snippet-0")
            .asInstanceOf[HTMLInputElement].checked = show
          showSnippets() = show
          if (show)
            "showSnippets" +: openSnippets
          else
            openSnippets.filterNot(_ == "showSnippets")
        } else {
          openSnippets
        }

        // Asynchronously save setting
        queryWire().saveSnippetSettings(snippetSettings).call().foreach {
          case Left(err) => window.alert(err)
          case Right(_)  =>
            println("Successfully saved open snippets settings: " +
              snippetSettings.mkString(", "))
        }
      }
    )
  ).now


  def dynRender: Rx.Dynamic[TypedTag[LI]] = Rx {
    _subMenuSnippet(
      cb(0, span(span("S", textDecoration.underline), "how snippets"), showSnippets),
      hr,
      cb(1, "Molecule", showMolecule),
      cb(2, "Favorites", showFavorites),
      cb(3, "Cache", showCache),
      cb(4, "Datalog", showDatalog),
      cb(5, "Transaction", showTransaction),
      cb(6, "Entity", showEntity),
      cb(7, "Entity History", showEntityHistory),
      h5("Debugging", paddingTop := 10, paddingBottom := 10),
      cb(8, "Molecule Model", showModel),
      cb(9, "Molecule Query", showQuery),
      cb(10, "Columns", showColumns),
      cb(11, "Tree with attr names only", showTree1),
      cb(12, "Tree with attr definitions", showTree2),
      cb(13, "Full Tree", showTree3)
    )
  }
}
