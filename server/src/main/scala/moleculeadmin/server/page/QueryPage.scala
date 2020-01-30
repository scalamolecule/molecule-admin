package moleculeadmin.server.page

import scalatags.Text
import scalatags.Text.all._

object QueryPage extends Base {

  def apply(db: String): Text.TypedTag[String] = html(
    head(
      moleculeAdminTitle,
      favicon,
      bootstrap,

      fontAwesome,
      openIconic,

      mainCss,
      tableDataCss,
      dropdownCss,
      navdropDownCss,

      menuAimJs,
      highlightJs,
      clientJs
    ),

    body(
      script(s"QueryClient.load('$db')"),
      div(id := "scriptWrapper"),
    )
  )
}