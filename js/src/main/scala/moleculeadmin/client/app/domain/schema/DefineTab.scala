package moleculeadmin.client.app.domain.schema
import boopickle.Default._
import moleculeadmin.client.app.domain.schema.SchemaState._
import moleculeadmin.client.app.domain.schema.definition._
import moleculeadmin.shared.ast.schema.{Attr => Attr_, _}
import rx.{Ctx, Rx}
import scalatags.JsDom.all._


case class DefineTab(db: String, schema0: MetaSchema)
                    (implicit val ctx: Ctx.Owner) extends Base {

  type keepBooPickleImport = PickleState

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
            for (Part(_, part, _, _, nss) <- schema1.parts) yield SchemaTree(part, nss).render
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
                Part(_, `part`, _, _, nss) <- schema1.parts
                Ns(_, `ns`, _, _, _, attrs) <- nss
                Attr_(n, `attr`, card, attrType, enums, refNs, options, doc, attrGroup, _, _, _, _) <- attrs
              } yield
                AttributeOnlyEdit(db, schema1, part, ns, n, attr, card, attrType, enums, refNs, options, doc, attrs, attrGroup).render

            } else if (nss.nonEmpty) {
              val nsAlias = nss.last

              for {
                Part(_, `part`, _, _, nss) <- schema1.parts
                Ns(n, `nsAlias`, ns, nsDescr, _, attrs) <- nss
              } yield
                Attributes(db, schema1, nss, part, n, nsAlias, ns, nsDescr, attrs).render

            } else {

              for {
                Part(n, `part`, descr, _, nss) <- schema1.parts
              } yield
                Namespaces(db, schema1, n, part, descr, nss).render
            }
          }
        )
      )
    )
  }
}
