package moleculeadmin.client.app.logic.schema
import boopickle.Default._
import moleculeadmin.client.app.logic.schema.SchemaState._
import moleculeadmin.client.app.logic.schema.definition._
import moleculeadmin.shared.ast.metaSchema._
import rx.{Ctx, Rx}
import scalatags.JsDom.all._


case class DefineTab(db: String, schema0: MetaSchema)
                    (implicit val ctx: Ctx.Owner) extends Base {

  // Init
  schema() = schema0

  def render2 = Rx {
    // trigger recalculations when open/schema changes
    open()
    val schema1 = schema()

    table(
      marginLeft := 13,
      tr(

        // Schema tree menu
        td(verticalAlign.top,
          div(cls := "list-group",
            for (MetaPart(_, part, _, _, nss) <- schema1.parts) yield SchemaTree(part, nss).render
          )
        ),

        // Content body
        td(
          cls := "list-group",
          marginLeft := 20,
          verticalAlign.top,
          if (schema1.parts.nonEmpty && schema1.parts.head.name.isEmpty) {
            div("todo: no partition...")
          } else if (curPart().isEmpty) {
            Partitions(db, schema1).render
          } else {
            val (part, nss, activeAttr) = open.now.find(_._1 == curPart.now.last).get

            if (activeAttr.nonEmpty) {
              val (ns, attr) = (nss.last, activeAttr.get)
              for {
                MetaPart(_, `part`, _, _, nss) <- schema1.parts
                MetaNs(_, `ns`, _, _, _, attrs) <- nss
                MetaAttr(n, `attr`, card, attrType, enums, refNs, options, doc, attrGroup, _, _, _, _) <- attrs
              } yield
                AttributeOnlyEdit(db, schema1, part, ns, n, attr, card, attrType, enums, refNs, options, doc, attrs, attrGroup).render

            } else if (nss.nonEmpty) {
              val nsAlias = nss.last

              for {
                MetaPart(_, `part`, _, _, nss) <- schema1.parts
                MetaNs(n, `nsAlias`, ns, nsDescr, _, attrs) <- nss
              } yield
                Attributes(db, schema1, nss, part, n, nsAlias, ns, nsDescr, attrs).render

            } else {

              for {
                MetaPart(n, `part`, descr, _, nss) <- schema1.parts
              } yield
                Namespaces(db, schema1, n, part, descr, nss).render
            }
          }
        )
      )
    )
  }
}
