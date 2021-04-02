package moleculeadmin.client.app.logic.schema.definition
import boopickle.Default._
import moleculeadmin.shared.ast.metaSchema._
import rx.Ctx
import scalatags.JsDom.all._


case class AttributeOnlyEdit(db: String,
                             schema1: MetaSchema,
                             part: String,
                             ns: String,
                             pos: Int,
                             attr: String,
                             card: Int,
                             attrType: String,
                             enums: Seq[String],
                             refNs: Option[String],
                             options: Seq[String],
                             doc: Option[String],
                             attrs: Seq[MetaAttr],
                             attrGroup: Option[String])
                            (implicit val ctx: Ctx.Owner) extends Base {

  def render = div(
    Breadcrumb(db, part, ns, attr).render,
    AttributeForm(db, schema1, part, ns, pos, attr, card, attrType, enums, refNs, options, doc, attrs, attrGroup).render
  )
}
