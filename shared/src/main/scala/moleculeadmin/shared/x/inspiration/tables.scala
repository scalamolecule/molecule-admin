package moleculeadmin.shared.x.inspiration

import scalatags.Text.all._
object tables {

  def wrappingTable(tableHead: Option[Frag], contents: Frag*) = {
    table(
      cls := "table",
      tableLayout.fixed,
      styles.normalTxt
    )(
      tableHead,
      tbody(contents)
    )
  }

}
