package moleculeadmin.client.app.domain.schema.definition
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.schema.SchemaState._
import moleculeadmin.client.autowire.schemaWire
import moleculeadmin.shared.ast.schema._
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class Partitions(db: String, schema1: MetaSchema)(implicit val ctx: Ctx.Owner) extends Base {

  type keepBooPickleImport = PickleState

  def render = div(
    Breadcrumb(db, "", "", "").render,
    if (schema1.parts.isEmpty) {
      div(s"No partitions yet...", marginTop := 20)
    } else {
      //          div(
      table(cls := "table", width.auto, marginTop := 18)(
        thead(
          tr(
            th(width := 30),
            th("Partition"),
            th("Description"),
            th(),
            th("Namespaces", textAlign.right),
            th("Attributes", textAlign.right),
            th(width := 10),
            th("Entities", textAlign.right)
          )
        ),
        Rx {
          tbody(
            for {
              Part(n, part, partDescr0, partValueCount0, nss) <- schema1.parts
            } yield {
              val attrCount = nss.map(_.attrs.length).sum
              val partValueCount = if (partValueCount0.isEmpty) "" else thousands(partValueCount0.get)
              val partDescr = partDescr0.getOrElse("")
              val deletePartErr = err(s"delete-part-err-$part")

              tr(
                td(n, color := "darkgray"),
                td(a(href := "#",
                  part,
                  onclick := showPart(part)
                )),
                td(partDescr, paddingRight := 10),
                td(whiteSpace.nowrap,
                  a(href := "#", cls := "oi oi-pencil", color := "#66c166", paddingRight := 10,
                    onclick := showPart(part)
                  ),
                  if (partValueCount.isEmpty || partValueCount == "0") {
                    a(
                      href := "#",
                      Rx(if (processing() == part + n) _sync() else i(cls := "oi oi-delete oi-spin", color := "#de5454")),
                      onclick := { () =>
                        processing() = part + n
                        schemaWire().deletePartition(schema1, db, part).call().foreach {
                          case Left(errMsg)         =>
                            deletePartErr.innerHTML = errMsg
                          case Right(updatedSchema) =>
                            processing() = ""
                            schema() = updatedSchema
                        }
                      }
                    )
                  } else {
                    a(href := "#", cls := "oi oi-delete", color := "#bbb",
                      onclick := { () =>
                        window.alert(s"Can't delete partition `$part` having ${nss.size} namespaces. Please delete namespaces first.")
                      }
                    )
                  },
                  deletePartErr
                ),
                td(nss.length, textAlign.center),
                td(attrCount, textAlign.center),
                td(),
                td(partValueCount, textAlign.right, fontFamily := "courier", whiteSpace.nowrap)
              )
            }
          )
        }
      )
    },
    PartitionForm(db, schema1, 0, "", None).render // Create partition
  )
}
