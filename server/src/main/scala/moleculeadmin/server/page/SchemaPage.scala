package moleculeadmin.server.page

import scalatags.Text
import scalatags.Text.all._

object SchemaPage extends Base {

  def apply(db: String): Text.TypedTag[String] = html(
    head(
      moleculeAdminTitle,
      favicon,
      bootstrap,

      fontAwesome,
      openIconic,

      mainCss,
      dropdownCss,
      navdropDownCss,

      highlightJs,
      clientJs
    ),
    body(
      script(s"SchemaClient.load('$db')"),
      div(id := "scriptWrapper"),
    )
  )
}