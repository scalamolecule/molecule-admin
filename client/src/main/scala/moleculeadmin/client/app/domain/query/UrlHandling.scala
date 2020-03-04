package moleculeadmin.client.app.domain.query

import molecule.util.RegexMatching
import moleculeadmin.client.app.domain.query.QueryState.{curMolecule, pushUrlOntoHistoryStack}
import org.scalajs.dom.{PopStateEvent, window}
import scala.scalajs.js.URIUtils
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.shared.ast.tree.Tree
import moleculeadmin.shared.ops.transform.Molecule2Model
import rx.{Ctx, Var}
import scalatags.JsDom.all.s

trait UrlHandling extends RegexMatching {

  def pushUrl(): Unit = {
    val newUrl =
      window.location.protocol + "//" +
        window.location.host +
        window.location.pathname +
        "?db=" + db +
        "&m=" + URIUtils.encodeURI(curMolecule.now)

    if (pushUrlOntoHistoryStack) {
      //      println("pushUrl")
      window.history.pushState(null, "MoleculeAdmin", newUrl)
    } else {
      //      println("pushUrl next time")
      pushUrlOntoHistoryStack = true
    }
  }


  def urlParams: Map[String, String] = {
    window.location.search.tail.split("&")
      .map(URIUtils.decodeURIComponent).map {
      case r"db=(.*)$db" => "db" -> db
      case r"m=(.*)$m"   => "m" -> m
      case other         => throw new IllegalArgumentException(
        "Unexpected URL parameter/value pair: " + other)
    }.toMap
  }


  def prepareBrowserHistory(implicit ctx: Ctx.Owner): Unit = {
    window.addEventListener("popstate", (_: PopStateEvent) => {

      urlParams.get("m").fold {
        //        println("No m...")
        curMolecule() = ""
        tree() = Tree(Nil, Nil, Nil, Nil, Nil, Nil)
        modelElements() = Nil
      } { m =>

        //        println("m2: " + m)
        if (m != curMolecule.now) {

          //          println("curMolecule: " + curMolecule.now)

          pushUrlOntoHistoryStack = false
          savedQueries.find(_.molecule == m) match {
            case Some(q) => new Callbacks().useQuery(q)
            case None    => Molecule2Model(m) match {
              case Right(elements) => modelElements() = elements
              case Left(err)       => window.alert(s"Error using query: $err")
            }
          }
        }


      }
    })
  }

  def loadOptionalMolecule(implicit ctx: Ctx.Owner): Unit = {
    urlParams.get("m").foreach { m =>

      //      println("m1: " + m)

      //      pushUrlOntoHistoryStack = true
      savedQueries.find(_.molecule == m) match {
        case Some(q) =>
          //          println("q: " + q)
          new Callbacks().useQuery(q)

        case None =>
          Molecule2Model(m) match {
            case Right(elements) =>

              //              elements foreach println

              modelElements() = elements
            case Left(err)       => window.alert(s"Error using query: $err")
          }
      }
    }
  }
}
