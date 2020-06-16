package moleculeadmin.server.page

import scalatags.Text
import scalatags.Text.all._

object DbsPage extends Base {

  def apply(): Text.TypedTag[String] = html(
    head(
      moleculeAdminTitle,
      favicon,
      bootstrap,

      fontAwesome,
      openIconic,

      mainCss,
      clientJs
    ),
    body(
      script("DbsClient.load()")
    )
  )
}