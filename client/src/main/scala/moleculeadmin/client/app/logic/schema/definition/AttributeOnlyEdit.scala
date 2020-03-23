package moleculeadmin.client.app.logic.schema.definition
import boopickle.Default._
import moleculeadmin.shared.ast.schema.{Attr => Attr_, _}
import scalatags.JsDom.all._
import moleculeadmin.client.app.logic.schema.SchemaState._
import rx.Ctx


case class AttributeOnlyEdit(db: String,
                             schema1: MetaSchema,
                             part: String,
                             ns: String,
                             pos: Int,
                             attr: String,
                             card: Int,
                             attrType: String,
                             enums: Option[Set[String]],
                             refNs: Option[String],
                             options: Option[Set[String]],
                             doc: Option[String],
                             attrs: Seq[Attr_],
                             attrGroup: Option[String])
                            (implicit val ctx: Ctx.Owner) extends Base {

  def render = div(
    Breadcrumb(db, part, ns, attr).render,
    AttributeForm(db, schema1, part, ns, pos, attr, card, attrType, enums, refNs, options, doc, attrs, attrGroup).render
  )
}
