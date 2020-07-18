package moleculeadmin.server.page

import controllers.routes
import scalatags.Text
import scalatags.Text.all._
import scalatags.Text.tags2.title

trait Base {

  def js(path: String): Text.TypedTag[String] =
    script(tpe := "text/javascript", src := routes.Assets.at(path).url)

  def css(path: String): Text.TypedTag[String] =
    link(rel := "stylesheet", href := routes.Assets.at(path).url)


  val moleculeAdminTitle = title("MoleculeAdmin")

  val favicon = link(
    rel := "shortcut icon",
    tpe := "image/png",
    href := routes.Assets.at("images/favicon.ico").url
  )

  val bootstrap = Seq(
    js("lib/jquery/jquery.min.js"),
    js("lib/popper.js/dist/umd/popper.min.js"),
    js("lib/bootstrap/js/bootstrap.js"),
    css("lib/bootstrap/css/bootstrap.css")
  )


  // fonts ------------------------------

  val fontAwesome   = css("lib/font-awesome/css/all.css")
  val openIconic    = css("lib/open-iconic/font/css/open-iconic-bootstrap.css")

  // web-jar only downloads regular icons :-(
  //  val materialIcons = css("lib/material-icons/iconfont/material-icons.css")
  // Have to download via link
  val materialIcons = link(
    rel := "stylesheet",
    tpe := "text/css",
    href := "https://fonts.googleapis.com/css?family=Material+Icons|Material+Icons+Outlined|Material+Icons+Two+Tone|Material+Icons+Round|Material+Icons+Sharp"
  )


  // css --------------------------------

  val mainCss        = css("stylesheets/main2.css")
  val tableDataCss   = css("stylesheets/tableData.css")
  val dropdownCss    = css("stylesheets/dropdown-menu.css")
  val navdropDownCss = css("stylesheets/navdropdown.css")


  // js ---------------------------------

  val highlightJs = Seq(
    css("lib/highlightjs/styles/idea.css"),
    js("lib/highlightjs/highlight.pack.js")
  )

  val menuAimJs = Seq(
    js("javascripts/jquery.menu-aim.js"),
    js("javascripts/jquery.menu-aim.options.js")
  )

  val clientJs = js("client-fastopt.js")
}
