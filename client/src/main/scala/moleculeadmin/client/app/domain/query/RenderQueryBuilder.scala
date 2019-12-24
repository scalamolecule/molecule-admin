package moleculeadmin.client.app.domain.query
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.app.domain.query.QueryState.querySelection
import moleculeadmin.client.app.domain.query.builder.{QueryBranches, SchemaDropDown}
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.schema.MetaSchema
import org.scalajs.dom.Node
import rx.Ctx
import scalatags.JsDom.all.span

case class RenderQueryBuilder(db: String, metaSchema: MetaSchema)(implicit val ctx: Ctx.Owner)
  extends RxBindings with AppElements {

  def rxElement: Node = querySelection.map {
    case "" => span()
    case "m" => _rowColAuto6(QueryBranches("m").dynRender)
    case key =>
      //      println("QueryBuilder...")
      _rowColAuto(
        _row(
          _rowCol(SchemaDropDown(db, metaSchema, key).dynRender),
          // explicit render avoiding double implicit render
          _rowColAuto4(QueryBranches(key).dynRender)
        )
      )
  }.render
}
