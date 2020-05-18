package moleculeadmin.client.app.logic.query

import molecule.ast.model.Atom
import molecule.util.RegexMatching
import moleculeadmin.client.app.logic.query.QueryState.{curMolecule, pushUrlOntoHistoryStack, _}
import moleculeadmin.shared.ast.tree.Tree
import moleculeadmin.shared.ops.transform.Molecule2Model
import org.scalajs.dom.{PopStateEvent, window}
import rx.Ctx
import scala.scalajs.js.URIUtils

trait UrlHandling extends RegexMatching {

  def pushUrl(): Unit = {
    val m      = if (curMolecule.now.isEmpty) "" else {
      "&m=" + URIUtils.encodeURI(curMolecule.now.replace("\n", ""))
    }
    val newUrl =
      window.location.protocol + "//" +
        window.location.host +
        window.location.pathname +
        "?db=" + db + m

    if (pushUrlOntoHistoryStack) {
      window.history.pushState(null, "MoleculeAdmin", newUrl)
    } else {
      pushUrlOntoHistoryStack = true
    }
  }

  def urlParams: Map[String, String] = {
    window.location.search.tail.split("&")
      .map(URIUtils.decodeURIComponent).map {
      case r"db=(.*)$db" => "db" -> db
      case r"m=(.*)$m"   => "m" -> m
        .replace("%20", " ")
      case other         => throw new IllegalArgumentException(
        "Unexpected URL parameter/value pair: " + other)
    }.toMap
  }

  def setQuery(m: String)(implicit ctx: Ctx.Owner) = {
    pushUrlOntoHistoryStack = false

    // Try cached recent query first
    recentQueries.find(_.molecule == m) match {
      case Some(q) => new Callbacks().useQuery(q)

      case None =>
        // Then try saved query
        savedQueries.find(_.molecule == m) match {
          case Some(q) => new Callbacks().useQuery(q)
          case None    =>
            // Finally create from molecule
            Molecule2Model(m) match {
              case Right(elements) => modelElements() = elements
              case Left(err)       => window.alert(s"Error using query: $err")
            }
        }
    }
  }

  def prepareBrowserHistory(implicit ctx: Ctx.Owner): Unit = {
    window.addEventListener("popstate", (_: PopStateEvent) => {
      urlParams.get("m").fold {
        if (!modelElements.now.exists {
          case Atom(_, "Dummy to keep ns open", _, _, _, _, _, _) => true
          case _                                                  => false
        }) {
          curMolecule() = ""
          tree() = Tree(Nil, Nil, Nil, Nil, Nil, Nil)
          modelElements() = Nil
        }
      } { m =>
        if (m != curMolecule.now.replace("\n", "")) {
          setQuery(m)
        }
      }
    })
  }

  def loadOptionalMoleculeFromUrl(implicit ctx: Ctx.Owner): Unit = {
    urlParams.get("m").foreach(setQuery)
  }
}





