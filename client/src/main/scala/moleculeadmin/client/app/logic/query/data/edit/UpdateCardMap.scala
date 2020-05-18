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
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try


/**
 * @tparam T Map[String, String] / Map[String, Double]
 **/
case class UpdateCardMap[T](
  cols: Seq[Col],
  qr: QueryResult,
  origArray: Array[Option[T]],
  editArray: Array[Option[T]],
  baseClass: String,
  rowIndex: Int,
  related: Int,
  nsAlias: String,
  nsFull: String,
  attr: String,
  attrType: String,
  enums: Seq[String],
  expr: String
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

    val oldPairs: List[(String, String)] =
      oldVopt.fold(List.empty[(String, String)]) {
        case vs: Map[_, _] =>
          vs.map { case (k, v) => (k.toString, v.toString) }
            .toList.distinct.sortBy(_._1)
      }

    val raw  = cell.innerHTML
    val strs = if (raw.endsWith("<ul></ul>")) {
      raw.replace("<ul></ul>", "").split("<br>").toList
    } else
      raw
        .substring(8, raw.length - 10)
        .split("</li><li>")
        .toList

    val vs = if (attrType == "String" && enums.isEmpty) {
      strs
        .map(_
          .replace("&nbsp;", " ")
          .replace("&lt;", "<")
          .replace("&gt;", ">")
          .replace("&amp;", "&")
          .replace("<br>", "\n")
          .trim)
        .filter(_.nonEmpty)
        .map(_html2str)
    } else {
      strs.flatMap(
        _.split("<br>")
          .map(_
            .replace("&nbsp;", " ")
            .replace("&lt;", "<")
            .replace("&gt;", ">")
            .replace("&amp;", "&")
            .trim)
          .filter(_.nonEmpty)
      )
    }

    val newPairs: List[(String, String)] = vs.map { pair =>
      if (!pair.contains("->")) {
        window.alert("Key/value should be separated by '->'")
        return
      }
      val k = pair.substring(0, pair.indexOf("->")).trim
      val v = pair.substring(pair.indexOf("->") + 2).trim
      if (attrType == "Date")
        (k, truncateDateStr(v))
      else
        (k, v)
    }.toMap.toList // remove pairs with duplicate keys
      .sortBy(_._1)


    if (oldPairs == newPairs) {
      // Remove superfluous line shifts
      redrawCell()

      // Void change marker
      setCellEditMode(cell, oldVopt, oldVopt)

    } else if (eid != 0 && editCellId.nonEmpty && editCellId == cell.id) {

      val newVopt: Option[T] = {
        if (newPairs.isEmpty)
          Option.empty[T]
        else if (isNum)
          Some(newPairs.map {
            case (k, v) => (k, v.toDouble)
          }.toMap).asInstanceOf[Option[T]]
        else
          Some(newPairs.toMap).asInstanceOf[Option[T]]
      }

      val retracts = oldPairs.diff(newPairs).map { case (k, v) => s"$k@$v" }
      val asserts  = newPairs.diff(oldPairs).map { case (k, v) => s"$k@$v" }

      val newKeys       = newPairs.map(_._1)
      val duplicateKeys = newKeys.length - newKeys.distinct.length

      val retractsAsserts = (if (retracts.isEmpty) "" else
        retracts.mkString("\n  RETRACT: `", "`\n  RETRACT: `", "`")) +
        (if (asserts.isEmpty) "" else
          asserts.mkString("\n  ASSERT : `", "`\n  ASSERT : `", "`"))

      if (!newPairs.forall { case (k, v) => k.nonEmpty && v.nonEmpty }) {
        editCellId = ""
        window.alert(s"Can't update $attrFull with invalid pair(s): $retractsAsserts")
        cell.focus()

      } else if (!newPairs.forall(pair => valid(attrType, pair._2))) {
        editCellId = ""
        window.alert(s"Can't update $attrFull with invalid value(s): $retractsAsserts")
        cell.focus()

      } else if (duplicateKeys == 1) {
        editCellId = ""
        window.alert(s"Can't update $attrFull having two pairs with duplicate key: $retractsAsserts")
        cell.focus()

      } else if (duplicateKeys > 1) {
        editCellId = ""
        window.alert(s"Can't update $attrFull with $duplicateKeys duplicate key(s): $retractsAsserts")
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

        // Update edit cell
        redrawCell()

        // update db
        val data = Seq((eid, retracts, asserts))
        queryWireAjax().updateStr(db, attrFull, "String", "", data).call().foreach {
          case Right((t, tx, txInstant)) =>
            updateClient(cellIdMaker, t, tx, txInstant, row, eid, newVopt)
            println(s"Updated $attrFull: $retractsAsserts")

          case Left(err) =>
            editCellId = ""
            selectContent(cell)
            window.alert(s"Error updating $attrFull: $err $retractsAsserts")
            cell.focus()
        }
      }

    } else if (eid != 0) {
      if (!newPairs.forall(pair => valid(attrType, pair._2))) {
        window.alert(
          s"Invalid $attrFull values of type `$attrType`:\n  " +
            newPairs.mkString("\n  ")
        )
        cell.focus()
      } else {
        println(s"OBS: New $attrFull value will not be saved " +
          s"unless you leave cell by pressing Enter/Return. Values:\n  " +
          newPairs.mkString("\n  ")
        )
      }

    } else {
      // Remove superfluous line shifts
      redrawCell()
    }


    def redrawCell(): Node = {
      cell.innerHTML = ""
      val vs = if (attrType == "String")
        newPairs.sorted.map {
          case (k, v) => span(_str2frags(k + " -> " + v))
        }
      else
        newPairs.sorted.map {
          case (k, v) => span(k + " -> " + v)
        }

      vs.foreach { v =>
        cell.appendChild(v.render)
        cell.appendChild(br.render)
      }
      cell
    }
  }
}
