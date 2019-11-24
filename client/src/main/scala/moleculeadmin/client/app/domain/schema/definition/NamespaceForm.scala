package moleculeadmin.client.app.domain.schema.definition
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.schema.SchemaState._
import moleculeadmin.client.autowire.schemaWire
import moleculeadmin.shared.ast.schema._
import org.scalajs.dom.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.all._
import scalatags.generic.{Attr, AttrPair}
import scala.concurrent.ExecutionContext.Implicits.global


case class NamespaceForm(db: String,
                         schema1: MetaSchema,
                         nss: Seq[Ns],
                         part: String,
                         pos: Int,
                         nsAlias: String,
                         ns: String,
                         nsDescr0: Option[String])
                        (implicit val ctx: Ctx.Owner) extends Base {

  type keepBooPickleImport_NamespaceForm = PickleState

  def render = {
    val update = pos > 0
    def v(str: String): AttrPair[Element, String] = if (update) value := str else Attr("empty").empty

    val nsName = input(id := "ns-name", size := 20, v(nsAlias)).render
    val nsDescr = input(id := "ns-descr", size := 70, v(nsDescr0.getOrElse(""))).render

    val nsCount = nss.size
    val nsPos = select(id := "ns-pos",
      option(value := 1, "First"),
      if (update) {
        nss.filterNot(_.name == nsAlias).map {
          case ns1 if ns1.pos == pos - 1 => option(value := ns1.pos + 1, ns1.name, selected := true)
          case ns1 if ns1.pos > pos      => option(value := ns1.pos, ns1.name)
          case ns1                       => option(value := ns1.pos + 1, ns1.name)
        }
      } else {
        nss.map {
          case ns1 if ns1.pos == nsCount => option(value := ns1.pos + 1, ns1.name, selected := true)
          case ns1                       => option(value := ns1.pos + 1, ns1.name)
        }
      }
    ).render

    val submitButton = button(id := "nsFormSubmit", tpe := "submit", if (update) "Update namespace" else "Create namespace").render

    val nsNameErr = err("ns-name-err")
    val submitErr = err("ns-err")

    def valid(): Boolean = {
      val nsName1 = nsName.value.trim
      val err = if (nsName1.isEmpty)
        "Please enter a namespace name matching [A-Z][a-zA-Z0-9]*"
      else if (nsName1.head.isLower || nsName1.head.isDigit)
        "Namespace name should start with an uppercase letter and match [A-Z][a-zA-Z0-9]*."
      else if (nsName1.contains(' '))
        "Namespace name has to be a single word (no spaces) and match [A-Z][a-zA-Z0-9]*."
      else if (!nsName1.matches("[A-Z][a-zA-Z0-9]*"))
        "Namespace name has to match [A-Z][a-zA-Z0-9]*"
      else if ((update && nsName1 != nsAlias && nss.exists(_.name == nsName1)) || (!update && nss.exists(_.name == nsName1)))
        s"Namespace name `$nsName1` already exists. Please enter another name."
      else
        ""
      if (err.nonEmpty) {
        processing() = ""
        nsNameErr.innerHTML = err
        submitButton.disabled = false
        nsName.select()
        false
      } else {
        nsNameErr.innerHTML = ""
        true
      }
    }

    form(
      _row(
        _rowColAuto2(if (update) "Namespace" else "New Namespace",
          br, nsName,
          br, nsNameErr
        ),
        _rowColAuto2("Description", color := "#777",
          br, nsDescr
        ),
        if (nss.nonEmpty) _rowColAuto2(div("After", paddingLeft := 5), nsPos) else (),
        _rowColAuto2(
          br, div(whiteSpace.nowrap, submitButton, Rx(if (processing() == "ns") _sync(15) else div())),
          submitErr
        )
      ),

      onchange := { () => valid() },
      onsubmit := { () =>
        // Avoid resubmission
        submitButton.disabled = true

        // activate spinner
        processing() = "ns"

        if (valid()) {
          val descr = nsDescr.value match {
            case " "    => Some("")
            case ""     => None
            case descr1 => Some(descr1.trim)
          }

          if (update) {
            schemaWire().updateNamespace(schema1, db, part, nsAlias, nsName.value, descr, nsPos.value.toInt).call().foreach {
              case Left(errMsg) =>
                processing() = ""
                submitErr.innerHTML = errMsg
                submitButton.disabled = false
                nsName.select()

              case Right(updatedSchema) =>
                processing() = ""
                // close namespace and go back to partition with namespace list
                open() = open.now.map {
                  case (`part`, nss1, _) => (part, nss1.filterNot(_ == nsAlias), None)
                  case (part1, nss1, _)  => (part1, nss1, None)
                }
                schema() = updatedSchema
            }
          } else {
            schemaWire().createNamespace(schema1, db, part, nsName.value, descr, nsPos.value.toInt).call().foreach {
              case Left(errMsg) =>
                processing() = ""
                submitErr.innerHTML = errMsg
                submitButton.disabled = false
                nsName.select()

              case Right(updatedSchema) =>
                processing() = ""
                //                  nsName.value = ""
                submitButton.disabled = false
                schema() = updatedSchema
            }
          }
        }
        false // don't send form / reload page - all rendering is triggered by Rx variables being updated
      } // onsubmit
    )
  }
}
