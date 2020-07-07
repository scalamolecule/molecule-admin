package moleculeadmin.client.app.logic.query.data.edit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState.{curEntity, db, editCellId}
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import org.scalajs.dom.html.{TableCell, TableRow}
import org.scalajs.dom.raw.Node
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


/**
 * @tparam T List[String] / List[Double]
 **/
case class UpdateCardMany[T](
  cols: Seq[Col],
  qr: QueryResult,
  origArray: Array[Option[T]],
  editArray: Array[Option[T]],
  baseClass: String,
  colType: String,
  rowIndex: Int,
  related: Int,
  nsAlias: String,
  nsFull: String,
  attr: String,
  attrType: String,
  enums: Seq[String],
  cellType: String,
  kind: String
)(implicit ctx: Ctx.Owner)
  extends UpdateClient[T](
    cols, qr, editArray, baseClass,
    rowIndex, related, nsAlias, nsFull, attr, enums
  ) {

  def update(
    cellIdMaker: Int => String,
    cellId: String,
    cell: TableCell,
    row: TableRow,
    eid: Long,
    isNum: Boolean,
    isGroupEdit: Boolean
  ): Unit = {

    val oldVopt = if (isGroupEdit) origArray(rowIndex) else editArray(rowIndex)

    val oldStrs: List[String] = oldVopt.fold(List.empty[String]) {
      case vs: List[_] =>
        vs.map(_.toString).distinct.sorted
    }

    val raw  = cell.innerHTML
    val strs = if (cellType == "ref") {
      val s1 = raw.replaceAll("</*ul[^>]*>", "")
      s1.substring(s1.indexOf(">", 5) + 1, s1.length - 5)
        .replaceAll("(<br>)*$", "")
        .split("""</li><li class="eid(Chosen)*">""").toList
    } else if (raw.endsWith("<ul></ul>")) {
      // Consider line shifts not as new item but as line shift within string
      List(raw.replace("<ul></ul>", ""))
    } else if (cellType == "str") {
      val before = "<ul><li>".size
      val after  = "</li></ul>".size
      raw.substring(before, raw.length - after).split("</li><li>").toList
    } else {
      raw.split("<br>").toList
    }

    val vs = _decode(strs, attrType, enums)

    val newStrs: List[String] = vs.distinct.sorted


    if (oldStrs == newStrs) {
      // Remove superfluous line shifts
      redrawCell()

      // Void change marker
      setCellEditMode(cell, oldVopt, oldVopt)

    } else if (eid != 0 && editCellId.nonEmpty && editCellId == cell.id) {

      val newVopt: Option[T] = {
        if (newStrs.isEmpty)
          Option.empty[T]
        else if (isNum)
          Some(newStrs.map(_.toDouble)).asInstanceOf[Option[T]]
        else
          Some(newStrs).asInstanceOf[Option[T]]
      }

      val (retracts, asserts) = (oldStrs.diff(newStrs), newStrs.diff(oldStrs))

      val retractsAsserts = (if (retracts.isEmpty) "" else
        retracts.mkString("\n  RETRACT: `", "`\n  RETRACT: `", "`")) +
        (if (asserts.isEmpty) "" else
          asserts.mkString("\n  ASSERT : `", "`\n  ASSERT : `", "`"))

      if (enums.nonEmpty && !newStrs.forall(enums.contains(_))) {
        editCellId = ""
        window.alert(
          s"Can't update $attrFull with non-defined enum value(s): $retractsAsserts" +
            "\nThe following enum values are defined:\n  " +
            enums.mkString("\n  ")
        )
        cell.focus()

      } else if (!newStrs.forall(valid(attrType, _))) {
        editCellId = ""
        window.alert(s"Can't update $attrFull with invalid value(s): $retractsAsserts")
        cell.focus()

      } else if (kind == "edit") {
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

        // Update edit cell
        redrawCell()

        // update db
        val dbUpdate = if (colType == "listDouble") {
          val data = Seq((eid, retracts.map(_.toDouble), asserts.map(_.toDouble)))
          queryWireAjax().updateNum(db, attrFull, attrType, data).call()
        } else {
          val data = Seq((eid, retracts, asserts))
          queryWireAjax().updateStr(db, attrFull, attrType, enumPrefix, data).call()
        }
        dbUpdate.foreach {
          case Right((t, tx, txInstant)) =>
            updateClient(cellIdMaker, t, tx, txInstant, row, eid, newVopt)
            println(s"Updated $attrFull $retractsAsserts")

          case Left(err) =>
            editCellId = ""
            selectContent(cell)
            window.alert(s"Error updating $attrFull: $err $retractsAsserts")
            cell.focus()
        }
      }

    } else if (eid != 0) {
      if (!newStrs.forall(valid(attrType, _))) {
        window.alert(
          s"Invalid $attrFull values of type `$attrType`:\n  " +
            newStrs.mkString("\n  ")
        )
        cell.focus()
      } else {
        println(s"OBS: New $attrFull value will not be saved " +
          s"unless you leave cell by pressing Enter/Return. Values:\n  " +
          newStrs.mkString("\n  ")
        )
      }

    } else {
      // Remove superfluous line shifts
      redrawCell()
    }

    def redrawCell(): Node = {
      cell.innerHTML = ""
      val vs: Seq[Frag] = if (cellType == "ref") {
        newStrs.map(_.toLong).sorted.map { ref =>
          span(
            cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
            ref,
            onmouseover := { () => curEntity() = ref }
          )
        }
      } else if (isNum) {
        newStrs.map(_.toDouble).sorted.flatMap(Seq(_, br))
      } else if (attrType == "String" && enums.isEmpty) {
        Seq(ul(newStrs.sorted.map(s => li(_str2frags(s)))))
      } else {
        newStrs.sorted.flatMap(Seq(_, br))
      }
      vs.foreach(v => cell.appendChild(v.render))
      cell
    }
  }
}
