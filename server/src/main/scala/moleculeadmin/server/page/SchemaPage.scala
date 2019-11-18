package moleculeadmin.server.page

import controllers._
import org.webjars.play.WebJarsUtil
import scalatags.Text.all.{body, _}
import scalatags.Text.tags2.title

object SchemaPage {

  def apply(db: String)(implicit webJarsUtil: WebJarsUtil) = html(
    head(
      title("MoleculeAdmin"),
      link(rel := "shortcut icon", tpe := "image/png", href := routes.Assets.versioned("images/favicon.ico").url),

      // Bootstrap
      raw(webJarsUtil.locate("jquery.min.js").script().body),
      raw(webJarsUtil.locate("dist/umd/popper.min.js").script().body),
      raw(webJarsUtil.locate("bootstrap.js").script().body),
      raw(webJarsUtil.locate("bootstrap.css").css().toString),

      raw(webJarsUtil.locate("fontawesome.css").css().toString),
      raw(webJarsUtil.locate("solid.css").css().toString),
      raw(webJarsUtil.locate("idea.css").css().toString),
      raw(webJarsUtil.locate("highlight.pack.js").script().body),

      link(rel := "stylesheet", href := routes.Assets.versioned("/fonts/open-iconic/font/css/open-iconic-bootstrap.css").url),


      link(rel := "stylesheet", href := routes.Assets.versioned("stylesheets/main2.css").url),
      link(rel := "stylesheet", href := routes.Assets.versioned("stylesheets/dropdown-menu.css").url),
      link(rel := "stylesheet", href := routes.Assets.versioned("stylesheets/navdropdown.css").url),


      script(tpe := "text/javascript", src := routes.Assets.versioned("client-fastopt.js").url)
    ),

    body(
      script(s"SchemaClient.load('$db')"),
      div(id := "scriptWrapper"),
      div(id := "tooltipsWrapper"),
    )
  )
}