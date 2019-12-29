package moleculeadmin.client.app.domain.query.data.edit
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState.{curEntity, db, editCellId}
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import org.scalajs.dom.html.{TableCell, TableRow}
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom.all.{attr, _}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


/**
 * @tparam T String / Double
 **/
case class UpdateCardOne[T](
  cols: Seq[Col],
  qr: QueryResult,
  origArray: Array[Option[T]],
  valueArray: Array[Option[T]],
  baseClass: String,
  colType: String,
  rowIndex: Int,
  colIndex: Int,
  related: Int,
  nsAlias: String,
  nsFull: String,
  attrName: String,
  attrType: String,
  card: Int,
  enums: Seq[String],
  expr: String
)(implicit ctx: Ctx.Owner)
  extends UpdateClient[T](
    cols, qr, origArray, valueArray, baseClass,
    colType, rowIndex, colIndex, related,
    nsAlias, nsFull, attrName, attrType, card, enums
  ) {

  type keepBooPickleImport_UpdateCardOne = PickleState

  def update(
    cellId: String,
    cell: TableCell,
    row: TableRow,
    eid: Long,
    oldVOpt: Option[T],
    isNum: Boolean
  ): Unit = {

    val oldStr: String = oldVOpt.fold("")(_.toString)
    val newStr: String = _html2str(cell.innerHTML)

    // Optional value for in-memory Client value array
    val newVopt: Option[T] = {
      if (newStr.isEmpty) {
        Option.empty[T]
      } else if (isNum) {
        Try(newStr.toDouble) match {
          case Success(n) => Some(n).asInstanceOf[Option[T]]
          case Failure(_) =>
            // Catch invalid str in validation below
            None
        }
      } else {
        Some(newStr).asInstanceOf[Option[T]]
      }
    }

    if (editCellId.nonEmpty
      && editCellId == cell.id
      && eid > 0
      && oldStr != newStr
    ) {
      if (attrType != "String" && newStr.contains('\n')) {
        editCellId = ""
        window.alert(s"Can't save multiple $attrFull values of type `$attrType`:\n$newStr")
        cell.focus()

      } else if (enums.nonEmpty && !enums.contains(newStr)) {
        editCellId = ""
        window.alert(
          s"Can't save non-defined $attrFull enum value `$newStr`" +
            "\nThe following enum values are defined:\n  " +
            enums.mkString("\n  ")
        )
        cell.focus()

      } else if (!valid(attrType, newStr)) {
        editCellId = ""
        window.alert(s"Invalid $attrFull value of type `$attrType`:\n$newStr")
        cell.focus()

      } else if (expr == "edit") {
        println(s"$eid $attrFull $newVopt")
        if (related == 0) {
          valueArray(rowIndex) = newVopt
          setCellEditMode(cell, newVopt)

        } else {
          // todo: should we allow group editing related values at all?
          val eArray = qr.num(qr.arrayIndexes(eIndex))
          var i      = 0
          while (i < eArray.length) {
            eArray(i) match {
              case Some(`eid`) => valueArray(i) = newVopt
              case _           =>
            }
            i += 1
          }
        }

      } else {
        // Raw collapsed string has visible characters
        val update = cell.textContent.trim.nonEmpty
        val value  = if (update) newStr else oldStr

        // Update value cell
        cell.innerHTML = ""
        if (update) {
          if (attrType == "ref") {
            // Need to replace whole cell, otherwise we can't
            // assign new Rx to class name - wonder why...
            val ref     = value.toLong
            val newCell = td(
              cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
              ref,
              id := cellId,
              attr("card") := 1,
              attr("eid") := eid,
              contenteditable := true,
              onmouseover := { () => curEntity() = ref },
              onblur := cell.onblur
            ).render
            val oldCell = row.childNodes.item(1 + colIndex)
            row.replaceChild(newCell, oldCell)
          } else if (attrType == "String") {
            cell.appendChild(_str2frags(value).render)
          } else {
            cell.appendChild(value.render)
          }
        }

        // update db
        val save = if (colType == "double") {
          val data = if (update)
            Seq((eid, Nil, Seq(value.toDouble)))
          else
            Seq((eid, Seq(value.toDouble), Nil))
          queryWire().updateNum(db, attrFull, attrType, data).call()
        } else {
          val data = if (update)
            Seq((eid, Nil, Seq(value)))
          else
            Seq((eid, Seq(value), Nil))
          queryWire().updateStr(db, attrFull, attrType, enumPrefix, data).call()
        }
        save.map {
          case Right((t, tx, txInstant)) =>
            updateClient(
              t, tx, txInstant,
              cellId, cell, row, eid, oldVOpt, isNum, newVopt
            )
            if (update)
              println(s"Successfully saved $attrFull value `$newStr`")
            else
              println(s"Successfully retracted $attrFull value `$oldStr`")

          case Left(err) =>
            editCellId = ""
            selectContent(cell)
            if (update)
              window.alert(s"Error saving $attrFull value `$newStr`:\n$err")
            else
              window.alert(s"Error retracting $attrFull value `$oldStr`:\n$err")
            cell.focus()
        }
      }
    } else if (eid > 0 && oldStr != newStr) {
      if (!valid(attrType, newStr)) {
        window.alert(s"Invalid $attrFull value of type `$attrType`:\n$newStr")
        cell.focus()
      } else {
        println(s"OBS: New $attrFull value `$newStr` will not be saved " +
          s"unless you leave cell by pressing Enter/Return.")
      }
    }
    // do nothing if no change
  }
}
