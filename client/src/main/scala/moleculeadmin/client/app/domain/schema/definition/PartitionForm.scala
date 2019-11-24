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


case class PartitionForm(db: String,
                         schema1: MetaSchema,
                         pos: Int,
                         part: String,
                         partDescr0: Option[String])(implicit val ctx: Ctx.Owner) extends Base {

  type keepBooPickleImport_PartitionForm = PickleState

  def render = {
    val update = pos > 0
    def v(str: String): AttrPair[Element, String] = if (update) value := str else Attr("empty").empty

    val partName = input(id := "part-name", size := 10, v(part)).render
    val partDescr = input(id := "part-descr", size := 30, v(partDescr0.getOrElse(""))).render
    val partCount = schema1.parts.size
    val partPos = select(id := "part-pos",
      option(value := 1, "First"),
      if (update) {
        schema1.parts.filterNot(_.name == part).map {
          case p if p.pos == pos - 1 => option(value := p.pos + 1, p.name, selected := true)
          case p if p.pos > pos      => option(value := p.pos, p.name)
          case p                     => option(value := p.pos + 1, p.name)
        }
      } else {
        schema1.parts.map {
          case p if p.pos == partCount => option(value := p.pos + 1, p.name, selected := true)
          case p                       => option(value := p.pos + 1, p.name)
        }
      }
    ).render
    val submitButton = button(id := "partFormSubmit", tpe := "submit", if (update) "Update partition" else "Create partition").render

    val partErr = err("part-err")
    val partNameErr = err("part-name-err")

    def valid(): Boolean = {
      val partName1 = partName.value.trim
      val err = if (partName1.isEmpty)
        "Please enter a partition name (has to match [a-z][a-z0-9]*)."
      else if (partName1.head.isUpper)
        "Partition name should start with a lowercase letter (has to match [a-z][a-z0-9]*)."
      else if (partName1.head.isDigit)
        "Partition name can't start with a digit (has to match [a-z][a-z0-9]*)."
      else if (partName1.contains(' '))
        "Partition name has to be a single word (has to match [a-z][a-z0-9]*)."
      else if (Seq("tx", "db").contains(partName1))
        "Partition can't have reserved name `tx` or `db` (has to match [a-z][a-z0-9]*)."
      else if (!partName1.matches("[a-z][a-z0-9]*"))
        "Partition name can only contain standard english letters a-z and digits 0-9."
      else if ((update && partName1 != part && schema1.parts.exists(_.name == partName1)) || (!update && schema1.parts.exists(_.name == partName1)))
        s"Partition name `$partName1` already exists. Please enter another name (matching [a-z][a-z0-9]*)."
      else
        ""
      if (err.nonEmpty) {
        processing() = ""
        partNameErr.innerHTML = err
        submitButton.disabled = false
        partName.select()
        false
      } else {
        partNameErr.innerHTML = ""
        true
      }
    }

    partName.select()

    form(
      _row(
        _rowColAuto2(if (update) "Partition" else "New partition",
          br, partName,
          br, partNameErr
        ),
        _rowColAuto2("Description", color := "#777",
          br, partDescr
        ),
        if (schema1.parts.nonEmpty)
          _rowColAuto2(div("After", paddingLeft := 5),
            partPos
          ) else (),
        _rowColAuto2(
          br, div(whiteSpace.nowrap, submitButton, Rx(if (processing() == "part") _sync(15) else div())),
          partErr
        )
      ),

      onchange := { () => valid() },
      onsubmit := { () =>
        // Avoid resubmission
        submitButton.disabled = true

        // activate spinner
        processing() = "part"

        if (valid()) {
          val descr = partDescr.value match {
            case " "    => Some("")
            case ""     => None
            case descr1 => Some(descr1.trim)
          }
          val submittedPart = partName.value.trim

          if (update) {
            schemaWire().updatePartition(schema1, db, part, submittedPart, descr, partPos.value.toInt).call().foreach {
              case Left(errMsg) =>
                processing() = ""
                partErr.innerHTML = errMsg
                submitButton.disabled = false
                partName.select()

              case Right(updatedSchema) =>
                processing() = ""
                // close partition and go back to partition list
                open() = Nil
                curPart() = Nil
                //                  open() = open.now.filterNot(_._1 == part) //:+ (submittedPart, Nil, None)
                //                  curPart() = curPart.now.filterNot(_ == part) //:+ submittedPart
                schema() = updatedSchema
            }

          } else {
            schemaWire().createPartition(schema1, db, submittedPart, descr, partPos.value.toInt).call().foreach {
              case Left(errMsg) =>
                processing() = ""
                partErr.innerHTML = errMsg
                submitButton.disabled = false
                partName.select()

              case Right(updatedSchema) =>
                processing() = ""
                partName.value = ""
                submitButton.disabled = false
                schema() = updatedSchema
            }
          }
        }
        false // don't send form / reload page
      } // onsubmit
    )
  }
}
