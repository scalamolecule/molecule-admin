package moleculeadmin.client.inspiration

import java.nio.ByteBuffer
import autowire._
import boopickle.Default.{Pickle, Pickler, Unpickle, _}
import org.scalajs.dom
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}


case class MyClient(repoName: String) extends autowire.Client[ByteBuffer, Pickler, Pickler] {
  override def doCall(req: Request): Future[ByteBuffer] = {
    dom.ext.Ajax.post(
      url = s"http://localhost:9001/$repoName/" + req.path.mkString("/"),
      data = Pickle.intoBytes(req.args),
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
  }

  override def read[Result: Pickler](p: ByteBuffer) = Unpickle[Result].fromBytes(p)
  override def write[Result: Pickler](r: Result) = Pickle.intoBytes(r)
}

object PathBar {

  trait RepoApi {
    def fetchSiblingNames(commitLabel: String, path: String): Seq[(Boolean, String)] = ???
  }

  def renderItem(repoName: String,
                 commitLabel: String,
                 partialPath: Seq[String],
                 linkAttrs: Seq[String] => (String, () => Unit)) = {
    renderBaseItem(
      partialPath.last,
      linkAttrs(partialPath),
      () => {
        val fetch: Future[Seq[(Boolean, String)]] = MyClient(repoName)[RepoApi].fetchSiblingNames(
          commitLabel, partialPath.mkString("/")
        ).call()
        for (xs <- fetch) yield {
          for ((isFolder, name) <- xs)
            yield {
              val icon = SharedTemplates.devopsIcon("git")
              PathBar.dropdownItem(frag(icon, " ", name), linkAttrs(partialPath))
            }
        }
      }
    )
  }

  def renderItem2(repoName: String,
                  commitLabel: String,
                  partialPath: Seq[String],
                  linkAttrs: Seq[String] => (String, () => Unit)) = {
    renderBaseItem(
      partialPath.last,
      linkAttrs(partialPath),
      () => {
        MyClient(repoName)[RepoApi].fetchSiblingNames(
          commitLabel, partialPath.mkString("/")
        ).call().map { xs =>
          for ((isFolder, name) <- xs)
            yield {
              val icon = SharedTemplates.devopsIcon("git")
              PathBar.dropdownItem(frag(icon, " ", name), linkAttrs(partialPath))
            }
        }
      }
    )
  }

  def renderItem3(repoName: String,
                  commitLabel: String,
                  partialPath: Seq[String],
                  linkAttrs: Seq[String] => (String, () => Unit)) = {
    renderBaseItem(
      partialPath.last,
      linkAttrs(partialPath),
      () => MyClient(repoName)[RepoApi].fetchSiblingNames(
        commitLabel, partialPath.mkString("/")
      ).call().map { xs =>
        for ((isFolder, name) <- xs) yield {
          val icon = SharedTemplates.devopsIcon("git")
          PathBar.dropdownItem(frag(icon, " ", name), linkAttrs(partialPath))
        }
      }
    )
  }

  def renderItem4(repoName: String,
                  commitLabel: String,
                  partialPath: Seq[String],
                  linkAttrs: Seq[String] => (String, () => Unit)) = {

    val items: () => Future[JsDom.Frag] = () => MyClient(repoName)[RepoApi].fetchSiblingNames(
      commitLabel, partialPath.mkString("/")
    ).call().map { xs =>
      for ((isFolder, name) <- xs) yield {
        val icon = SharedTemplates.devopsIcon("git")
        PathBar.dropdownItem(frag(icon, " ", name), linkAttrs(partialPath))
      }
    }

    renderBaseItem(partialPath.last, linkAttrs(partialPath), items)
  }


  def dropdownItem(frag: Any, xx: (String, () => Unit)): Frag = ???

  def renderRootItem(repoName: String,
                     navigationUrl: (String, String) => String,
                     linkAttrs: (String, () => Unit),
                     allRepoPrimaryBranchNames: Seq[(String, String)]) = {
    renderBaseItem(
      b(SharedTemplates.devopsIcon("git"), " ", repoName),
      linkAttrs,
      () => Future.successful(
        for ((otherRepoName, otherRepoBranch) <- allRepoPrimaryBranchNames) yield {
          dropdownItem(
            frag(SharedTemplates.devopsIcon("git"), " ", otherRepoName),
            (navigationUrl(otherRepoName, otherRepoBranch), null)
          )
        }
      )
    )
  }

  def renderBaseItem(txt: Frag,
                     linkAttrs: (String, () => Unit),
                     asyncDropdownContents: () => Future[Frag]) = {
    lazy val dropdown: dom.html.Div = div(
      display.none,

      marginTop := 0,
//      styles.dropdown,
      marginLeft := -10,
      role := "menu",
      maxHeight := 512,
      overflow.auto
    ).render

    dropdown
  }
}
