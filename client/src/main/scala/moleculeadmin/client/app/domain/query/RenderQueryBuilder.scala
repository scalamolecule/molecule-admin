package moleculeadmin.client.app.domain.query

import moleculeadmin.client.app.domain.query.QueryState.{queryBaseSelection, querySelection}
import moleculeadmin.client.app.domain.query.builder.{QueryBranches, SchemaDropDown}
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.app.element.query.SchemaDropdownElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.schema.MetaSchema
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.Node
import rx.Ctx
import scalatags.JsDom.all._

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
            SchemaDropDown(metaSchema, key).dynRender,
            _selection(selection)
          ),
          _rowColAuto4(QueryBranches(key).dynRender)
        )
      )
  }.render
}
