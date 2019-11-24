package moleculeadmin.client.app.domain.schema.definition
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.schema.SchemaState._
import moleculeadmin.client.autowire.schemaWire
import moleculeadmin.shared.ast.schema.{Attr => Attr_, _}
import org.scalajs.dom.html.TableRow
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class Attributes(db: String,
                      schema1: MetaSchema,
                      nss: Seq[Ns],
                      part: String,
                      pos: Int,
                      nsAlias: String,
                      ns: String,
                      nsDescr: Option[String],
                      attrs: Seq[Attr_])
                     (implicit val ctx: Ctx.Owner) extends Base {

  type keepBooPickleImport_Attributes = PickleState

  def render = div(
    Breadcrumb(db, part, nsAlias, "").render,
    NamespaceForm(db, schema1, nss, part, pos, nsAlias, ns, nsDescr).render, // Update namespace
    Rx {
      val deleteAttrErr = err("delete-err")
      val hasEnums = attrs.exists(_.enums$.getOrElse(Nil).nonEmpty)
      val hasOptions = attrs.exists(_.options$.getOrElse(Nil).exists(_ != "indexed"))
      if (attrs.isEmpty) {
        div(s"Create first attribute in namespace `$nsAlias`:", marginTop := 20)
      } else {
        table(cls := "table", width.auto,
          marginTop := 20
        )(
          thead(
            tr(
              th(width := 30),
              th("Attribute"),
              th("Card/Type"),
              if (hasEnums) th("Enums") else (),
              if (hasOptions) th("Options") else (),
              th("Description"),
              th(),
              th("Entities", textAlign.right),
              th("Values", textAlign.right)
            )
          ),
          tbody(
            attrs.flatMap { case Attr_(n, attr, card, attrType0, enums0, ref, options0, doc0, attrGroup, entityCount0, distinctValueCount0, _, _) =>
              val entityCount = if (entityCount0.isEmpty) "" else thousands(entityCount0.get)
              val distinctValueCount = if (distinctValueCount0.isEmpty) "" else thousands(distinctValueCount0.get)
              val cardPrefix = card match {
                case 1 => "one"
                case 2 => "many"
                case 3 => "map"
              }
              val attrType = ref match {
                case Some(refNsFull) if refNsFull.contains('_') =>
                  val refNsSplit = refNsFull.split('_')
                  val (refPart, refNs) = (refNsSplit(0), refNsSplit(1))
                  val refNsLabel = if (refPart == part) refNs else refNsFull
                  div(cardPrefix, "[", a(href := "#", refNsLabel, onclick := showNs(refPart, refNs)), "]")
                case Some(refNs)                                => div(cardPrefix, "[", a(href := "#", refNs, onclick := showNs(part, refNs)), "]")
                case None if enums0.nonEmpty                    => div(cardPrefix, "Enum")
                case None                                       => div(cardPrefix, attrType0.capitalize)
              }
              val enums = if (enums0.isEmpty) "" else {
                val enumsSorted = enums0.get.toSeq.sorted
                val first = enumsSorted.take(3).mkString(", ")
                val more = if (enumsSorted.size <= 3) "" else " + " + (enumsSorted.size - 3) + " more..."
                first + more
              }
              val options = if (options0.nonEmpty) options0.get.filterNot(_ == "indexed").mkString(", ") else ""
              val doc = doc0.getOrElse("")

              val optAttrGroupRow: Seq[JsDom.TypedTag[TableRow]] = attrGroup match {
                case Some("")    => Seq(tr(td(), td(cls := "attrGroupEmpty", colspan := "8", raw("&nbsp;"))))
                case Some(label) => Seq(tr(td(), td(cls := "attrGroup", colspan := "8", label)))
                case None        => Nil
              }

              optAttrGroupRow :+ tr(
                td(n, color := "darkgray"),
                td(a(href := "#", attr, onclick := showAttr(part, nsAlias, attr))),
                td(attrType),
                if (hasEnums) td(enums) else (),
                if (hasOptions) td(options) else (),
                td(doc, paddingRight := 10),
                td(whiteSpace.nowrap,
                  a(href := "#", cls := "oi oi-pencil", color := "#66c166", paddingRight := 10,
                    onclick := showAttr(part, nsAlias, attr)
                  ),
                  if (entityCount.isEmpty || entityCount == "0") {
                    a(
                      href := "#",
                      Rx(if (processing() == part + nsAlias + attr + n) _sync() else i(cls := "oi oi-delete oi-spin", color := "#de5454")),
                      onclick := { () =>
                        processing() = part + nsAlias + attr + n
                        schemaWire().deleteAttribute(schema1, db, part, nsAlias, attr).call().foreach {
                          case Left(errMsg)         => deleteAttrErr.innerHTML = errMsg
                          case Right(updatedSchema) => schema() = updatedSchema
                        }
                      }
                    )
                  } else {
                    a(
                      href := "#",
                      cls := "oi oi-delete", color := "#bbb",
                      onclick := { () =>
                        window.alert(s"Can't delete attribute `$attr` asserted with ${entityCount0.get} entities. Please delete entities first.")
                      }
                    )
                  },
                  deleteAttrErr
                ),
                td(entityCount, paddingLeft := 30, textAlign.right, fontFamily := "courier", whiteSpace.nowrap),
                td(distinctValueCount, paddingLeft := 30, textAlign.right, fontFamily := "courier", whiteSpace.nowrap)
              )

            }
          )
        )
      }
    },
    AttributeForm(db, schema1, part, nsAlias, 0, "", 0, "", None, None, None, None, attrs, None).render // Create attribute
  )
}
