package moleculeadmin.server.page

import controllers._
import org.webjars.play.WebJarsUtil
import scalatags.Text.all.{body, _}
import scalatags.Text.tags2.title

object DbsPage {

  def apply()(implicit webJarsUtil: WebJarsUtil) = html(
    head(
      title("MoleculeAdmin"),
      link(rel := "shortcut icon", tpe := "image/png", href := routes.Assets.versioned("images/favicon.ico").url),

      // Bootstrap
      raw(webJarsUtil.locate("jquery.min.js").script().body),
      raw(webJarsUtil.locate("dist/umd/popper.min.js").script().body),
      raw(webJarsUtil.locate("bootstrap.js").script().body),
      raw(webJarsUtil.locate("bootstrap.css").css().toString),

      link(rel := "stylesheet", href := routes.Assets.versioned("stylesheets/main2.css").url),
      script(tpe := "text/javascript", src := "versionedAssets/client-fastopt.js"),
    ),
    //    body(
//          bs.containerFluid(
//            bs.row(
//              display := "block",
//              Snippet.logo,
//              span("Molecule Admin", style := "margin-right: 20px; font-size: 20px")
//            ),
//            p(),
//            bs.row(content)
//          ),
    //      script("DbsClient.load()")
    //    )

    body(
      script("DbsClient.load()")
    )
  )
}