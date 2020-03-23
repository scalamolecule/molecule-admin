package moleculeadmin.client.app.logic.schema.definition
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.schema.SchemaState._
import moleculeadmin.client.schemaWire
import moleculeadmin.shared.ast.schema._
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class Namespaces(db: String,
                      schema1: MetaSchema,
                      pos: Int,
                      part: String,
                      partDescr: Option[String],
                      nss: Seq[Ns])(implicit val ctx: Ctx.Owner) extends Base {

  def render = div(
    Breadcrumb(db, part, "", "").render,
    PartitionForm(db, schema1, pos, part, partDescr).render, // Update partition
    Rx {
      if (nss.isEmpty) {
        div(s"No namespaces yet in partition `$part`...", marginTop := 20)
      } else {
        table(cls := "table", width.auto, marginTop := 20)(
          thead(
            tr(
              th(width := 30),
              th("Namespace"),
              th("Description"),
              th(),
              th("Attributes", textAlign.right),
              th("Entities", textAlign.right)
            )
          ),

          tbody(
            for {
              Ns(n, ns, _, nsDescr0, nsValueCount0, attrs) <- nss
            } yield {
              val nsValueCount = if (nsValueCount0.isEmpty) "" else thousands(nsValueCount0.get)
              val nsDescr = nsDescr0.getOrElse("")
              val deleteNsErr = err(s"delete-ns-err-$part-$ns")

              tr(
                td(n, color := "darkgray"),
                td(a(href := "#",
                  ns,
                  onclick := showNs(part, ns)
                )),
                td(nsDescr, paddingRight := 10),
                td(whiteSpace.nowrap,
                  a(href := "#", cls := "oi oi-pencil", color := "#66c166", paddingRight := 10,
                    onclick := showNs(part, ns)
                  ),
                  if (nsValueCount.isEmpty || nsValueCount == "0") {
                    a(
                      href := "#",
                      Rx(if (processing() == part + ns + n) _sync() else i(cls := "oi oi-delete oi-spin", color := "#de5454")),
                      onclick := { () =>
                        processing() = part + ns + n
                        schemaWire().deleteNamespace(schema1, db, part, ns).call().foreach {
                          case Left(errMsg)         => deleteNsErr.innerHTML = errMsg
                          case Right(updatedSchema) => schema() = updatedSchema
                        }
                      }
                    )
                  } else {
                    a(
                      href := "#",
                      cls := "oi oi-delete", color := "#bbb",
                      onclick := { () =>
                        window.alert(s"Can't delete namespace `$ns` having ${attrs.size} attributes. Please delete attributes first.")
                      }
                    )
                  },
                  deleteNsErr
                ),
                td(attrs.length, textAlign.center),
                td(nsValueCount, textAlign.right, fontFamily := "courier", whiteSpace.nowrap)
              )
            }
          )
        )
      }
    },
    NamespaceForm(db, schema1, nss, part, 0, "", "", None).render // Create namespace
  )
}
