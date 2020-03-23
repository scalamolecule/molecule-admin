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
    val m = if (curMolecule.now.isEmpty) "" else {
      "&m=" + URIUtils.encodeURI(
        curMolecule.now
          .replaceAllLiterally("\n", "")
      )
    }
    val newUrl =
      window.location.protocol + "//" +
        window.location.host +
        window.location.pathname +
        "?db=" + db + m

    if (pushUrlOntoHistoryStack) {
      //      println("pushUrl - " + curMolecule.now)
      window.history.pushState(null, "MoleculeAdmin", newUrl)
    } else {
      //      println("pushUrl next time - " + curMolecule.now)
      pushUrlOntoHistoryStack = true
    }
  }

  def urlParams: Map[String, String] = {
    window.location.search.tail.split("&")
      .map(URIUtils.decodeURIComponent).map {
      case r"db=(.*)$db" => "db" -> db
      case r"m=(.*)$m"   => "m" -> m
        .replaceAllLiterally("%20", " ")
      case other         => throw new IllegalArgumentException(
        "Unexpected URL parameter/value pair: " + other)
    }.toMap
  }

  def setQuery(m: String)(implicit ctx: Ctx.Owner) = {
    pushUrlOntoHistoryStack = false

    // Try cached recent query first
    recentQueries.find(_.molecule == m) match {
      case Some(q) =>
        //        println("  modelElements - setCols from recent q: " + q)
        new Callbacks().useQuery(q)

      case None =>
        // Then try saved query
        savedQueries.find(_.molecule == m) match {
          case Some(q) =>
            //            println("  modelElements - setCols from saved q: " + q)
            new Callbacks().useQuery(q)

          case None =>
            //            println("  Make from m")
            // Finally created from molecule
            Molecule2Model(m) match {
              case Right(elements) => modelElements() = elements
              case Left(err)       => window.alert(s"Error using query: $err")
            }
        }
    }
  }

  def prepareBrowserHistory(implicit ctx: Ctx.Owner): Unit = {
    window.addEventListener("popstate", (_: PopStateEvent) => {
      //      println("POPSTATE -----")
      urlParams.get("m").fold {
        //        println("  No m... " + modelElements.now)
        if (!modelElements.now.exists {
          case Atom(_, "Dummy to keep ns open", _, _, _, _, _, _) => true
          case _                                                  => false
        }) {
          //          println("  dummy")
          curMolecule() = ""
          tree() = Tree(Nil, Nil, Nil, Nil, Nil, Nil)
          modelElements() = Nil
        }
      } { m =>
        //        println("  m2           : " + m)
        if (m != curMolecule.now) {
          setQuery(m)
        }
      }
    })
  }

  def loadOptionalMolecule(implicit ctx: Ctx.Owner): Unit = {
    urlParams.get("m").foreach { m =>
      //      println("m1: " + m)
      setQuery(m)
    }
  }
}





