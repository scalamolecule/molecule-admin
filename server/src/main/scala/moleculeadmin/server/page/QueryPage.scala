package moleculeadmin.server.page

import controllers._
import org.webjars.play.WebJarsUtil
import scalatags.Text.all._
import scalatags.Text.tags2.title

object QueryPage {

  def apply(db: String)(implicit webJarsUtil: WebJarsUtil) = html(
    head(
      title("MoleculeAdmin"),
      link(rel := "shortcut icon", tpe := "image/png", href := routes.Assets.versioned("images/favicon.ico").url),

      // Bootstrap
      raw(webJarsUtil.locate("jquery.min.js").script().body),
      raw(webJarsUtil.locate("dist/umd/popper.min.js").script().body),
      raw(webJarsUtil.locate("bootstrap.js").script().body),
      raw(webJarsUtil.locate("bootstrap.css").css().toString),

      //      raw(webJarsUtil.locate("fontawesome.css").css().toString), // doesn't include the font, duh...
      raw(webJarsUtil.locate("all.css").css().toString), // font-awesome
      raw(webJarsUtil.locate("open-iconic-bootstrap.css").css().toString),

      link(rel := "stylesheet", href := routes.Assets.versioned("stylesheets/main2.css").url),
      link(rel := "stylesheet", href := routes.Assets.versioned("stylesheets/dropdown-menu.css").url),
      link(rel := "stylesheet", href := routes.Assets.versioned("stylesheets/tableData.css").url),
      link(rel := "stylesheet", href := routes.Assets.versioned("stylesheets/navdropdown.css").url),

      script(tpe := "text/javascript", src := routes.Assets.versioned("javascripts/jquery.menu-aim.js").url),
      script(tpe := "text/javascript", src := routes.Assets.versioned("javascripts/jquery.menu-aim.options.js").url),

      raw(webJarsUtil.locate("idea.css").css().toString),
      raw(webJarsUtil.locate("highlight.pack.js").script().body),

      script(tpe := "text/javascript", src := "/versionedAssets/client-fastopt.js")
    ),

    body(
      script(s"QueryClient.load('$db')"),
      div(id := "scriptWrapper"),
      div(id := "tooltipsWrapper"),
    )
  )
}