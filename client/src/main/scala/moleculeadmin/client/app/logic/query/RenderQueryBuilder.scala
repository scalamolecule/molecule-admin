package moleculeadmin.client.app.logic.query

import moleculeadmin.client.app.html.query.SchemaDropdownElements
import moleculeadmin.client.app.logic.query.QueryState.{queryBaseSelection, querySelection}
import moleculeadmin.client.app.logic.query.builder.{QueryBranches, SchemaDropdown}
import moleculeadmin.shared.ast.metaSchema.MetaSchema
import org.scalajs.dom.Node
import rx.Ctx
import scalatags.JsDom.all._
import util.client.rx.RxBindings

case class RenderQueryBuilder(metaSchema: MetaSchema)(implicit val ctx: Ctx.Owner)
  extends RxBindings with SchemaDropdownElements {

  def dynRender: Node = querySelection.map {
    case "" => span()

    case "m" =>
      _rowColAuto6(
        QueryBranches("m").dynRender
      )

    case key =>
      val selection = queryBaseSelection match {
        case "a" => "All"
        case "v" => "Values"
        case "r" => "No ref attrs"
      }
      _rowColAuto(
        _row(
          _rowCol(
            SchemaDropdown(metaSchema, key).dynRender,
            _selection(selection)
          ),
          _rowColAuto4(QueryBranches(key).dynRender)
        )
      )
  }.render
}
