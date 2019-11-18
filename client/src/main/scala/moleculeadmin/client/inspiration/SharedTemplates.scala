package moleculeadmin.client.inspiration

import scalatags.JsDom.all._

object SharedTemplates {
  def guessIconForFileName(name: String, value: Boolean) = "hi"

  def devopsIcon(name: String): Frag = {
    span(
      cls := s"devicons devicons-$name"
//      styles.devopIconStyle
    )
  }
}
