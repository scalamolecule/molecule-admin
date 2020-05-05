package moleculeadmin.client.app.logic.query.data.edit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState.{curEntity, db, editCellId}
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import org.scalajs.dom.html.{TableCell, TableRow}
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


/**
 * @tparam T String / Double
 **/
case class UpdateCardOne[T](
  cols: Seq[Col],
  qr: QueryResult,
  origArray: Array[Option[T]],
  editArray: Array[Option[T]],
  baseClass: String,
  colType: String,
  colIndex: Int,
  rowIndex: Int,
  related: Int,
  nsAlias: String,
  nsFull: String,
  attrName: String,
  attrType: String,
  enums: Seq[String],
  expr: String
)(implicit ctx: Ctx.Owner)
  extends UpdateClient[T](
    cols, qr, origArray, editArray, baseClass,
    rowIndex, related, nsAlias, nsFull, attrName, enums
  ) {

  def update(
    cellIdMaker: Int => String,
    cellId: String,
    cell: TableCell,
    row: TableRow,
    eid: Long,
    oldVopt: Option[T],
    isNum: Boolean
  ): Unit = {

    val oldStr: String = oldVopt.fold("")(_.toString)
    val newStr: String = _html2str(cell.innerHTML)

    if (oldStr == newStr) {
      // Void change marker
      setCellEditMode(cell, oldVopt, oldVopt)

    } else if (editCellId.nonEmpty && editCellId == cell.id && eid != 0) {

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

      if (attrType != "String" && newStr.contains('\n')) {
        editCellId = ""
        window.alert(
          s"Can't save multiple $attrFull values of type `$attrType`:\n$newStr")
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
          editArray(rowIndex) = newVopt
          setCellEditMode(cell, oldVopt, newVopt)

        } else {
          val eArray = qr.num(qr.arrayIndexes(eIndex))
          var i      = 0
          val length = eArray.length
          while (i < length) {
            eArray(i) match {
              case Some(`eid`) =>
                editArray(i) = newVopt
                setCellEditMode(cell, oldVopt, newVopt)
              case _           =>
            }
            i += 1
          }
        }

      } else {
        // Raw collapsed string has visible characters
        val nonEmpty = cell.textContent.trim.nonEmpty
        val value    = if (nonEmpty) newStr else oldStr

        // Update edit cell
        cell.innerHTML = ""
        if (nonEmpty) {
          if (attrType == "ref") {
            // Need to replace whole cell, otherwise we can't
            // assign new Rx to class name - wonder why...
            val ref     = value.toLong
            val newCell = td(
              id := cellId,
              cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
              ref,
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
        val dbUpdate = if (colType == "double") {
          val data = if (nonEmpty)
            Seq((eid, Nil, Seq(value.toDouble)))
          else
            Seq((eid, Seq(value.toDouble), Nil))
          queryWireAjax().updateNum(db, attrFull, attrType, data).call()
        } else {
          val data = if (nonEmpty)
            Seq((eid, Nil, Seq(value)))
          else
            Seq((eid, Seq(value), Nil))

          queryWireAjax().updateStr(db, attrFull, attrType, enumPrefix, data).call()
        }
        dbUpdate.foreach {
          case Right((t, tx, txInstant)) =>
            updateClient(cellIdMaker, t, tx, txInstant, row, eid, newVopt)
            if (nonEmpty)
              println(s"$attrFull: `$oldStr` ==> `$newStr`")
            else
              println(s"$attrFull: retracted `$oldStr`")

          case Left(err) =>
            editCellId = ""
            selectContent(cell)
            if (nonEmpty)
              window.alert(
                s"Error updating `$attrFull` value from `$oldStr` to `$newStr`:\n$err")
            else
              window.alert(s"Error retracting `$attrFull` value `$oldStr`:\n$err")
            cell.focus()
        }
      }

    } else if (eid != 0) {
      if (!valid(attrType, newStr)) {
        window.alert(s"Invalid `$attrFull` value of type `$attrType`:\n$newStr")
        cell.focus()
      } else {
        println(s"OBS: New `$attrFull` value `$newStr` will not be saved " +
          s"unless you leave cell by pressing Enter/Return.")
      }

    } else {
      // is eid ever 0?
    }
  }
}
