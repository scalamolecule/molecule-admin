package moleculeadmin.client.app.domain.query.grouped

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.edit.TypeValidation
import moleculeadmin.client.app.element.query.GroupedAttrElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.shared.ast.query.Col
import org.scalajs.dom.html.TableCell
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{NodeList, document, window}
import rx.Ctx
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


abstract class GroupedUpdate[T](col: Col)(implicit ctx: Ctx.Owner)
  extends GroupedData(col) with GroupedAttrElements with TypeValidation {

  type keepBooPickleImport_GroupedUpdate = PickleState

  def updateLambda(
    tableRows: NodeList,
    valueColIndex: Int,
    eidArray: Array[Option[Double]],
    valueArray: Array[Option[T]],
  ): Int => () => Unit = {
    (n: Int) =>
      () => {
        val (oldStr, count) = groupedData(n - 1)

        val groupedCell    = document.getElementById(s"grouped-cell-$colIndex-$n")
          .asInstanceOf[HTMLInputElement]
        val newStr: String = _html2str(groupedCell.innerHTML)

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
        val oldVopt: Option[T] =
          if (oldStr.isEmpty)
            None
          else if (colType == "double")
            Some(oldStr.toDouble.asInstanceOf[T])
          else
            Some(oldStr.asInstanceOf[T])

        if (editCellId.nonEmpty
          && editCellId == groupedCell.id
          && oldStr != newStr
        ) {
          if (attrType != "String" && newStr.contains('\n')) {
            editCellId = ""
            window.alert(
              s"Can't save multiple $attrFull values of type `$attrType`:\n$newStr"
            )
            groupedCell.focus()

          } else if (enums.nonEmpty && !enums.contains(newStr)) {
            editCellId = ""
            window.alert(
              s"Can't save non-defined $attrFull enum value `$newStr`" +
                "\nThe following enum values are defined:\n  " +
                enums.mkString("\n  ")
            )
            groupedCell.focus()

          } else if (!valid(attrType, newStr)) {
            editCellId = ""
            window.alert(s"Invalid $attrFull value of type `$attrType`:\n$newStr")
            groupedCell.focus()

          } else {
            val nonEmpty = groupedCell.textContent.trim.nonEmpty
            val value    = if (nonEmpty) newStr else oldStr

            // Update grouped data
            groupedData = groupedData.updated(n - 1, (value, count))

            // Update edit cell in grouped data table
            groupedCell.innerHTML = ""
            if (nonEmpty) {
              if (attrType == "String") {
                groupedCell.appendChild(_str2frags(value).render)
              } else {
                groupedCell.appendChild(value.render)
              }
            }

            // Update values in data table
            val (html2value, value2node) = attrType match {
              case "String" => (
                (html: String) => _html2str(html),
                (v: String) => _str2frags(v).render
              )
              case _        => (
                (html: String) => html,
                (v: String) => v.render
              )
            }

            var affectedRows = List.empty[Int]
            var i            = 0
            while (i < tableRows.length) {
              val cell  = tableRows.item(i).childNodes.item(valueColIndex).asInstanceOf[TableCell]
              val cellV = html2value(cell.innerHTML)
              if (oldStr == cellV) {
                affectedRows = affectedRows :+ i
                cell.innerHTML = ""
                cell.appendChild(value2node(newStr))
              }
              i += 1
            }

            // Update value array and collect entity ids
            val filterIndex = queryCache.filterIndex
            val indexBridge = {
              if (filterIndex.nonEmpty)
                (i: Int) => filterIndex(i)
              else
                (i: Int) => i
            }
            val length      = actualRowCount // (filtered)
            val positives   = new Array[Int](length)
            var eids        = List.empty[Long]
            i = 0
            var posIndex = 0
            var j        = 0
            while (i < length) {
              j = indexBridge(i)
              if (valueArray(j) == oldVopt) {
                eids = eidArray(j).get.toLong :: eids
                positives(posIndex) = j
                valueArray(j) = newVopt
                posIndex += 1
              }
              i += 1
            }

            val affectedIndexes = new Array[Int](posIndex)
            System.arraycopy(positives, 0, affectedIndexes, 0, posIndex)

            // Update db
            val save = if (colType == "double") {
              val v    = value.toDouble
              val data = if (nonEmpty)
                eids.map((_, Nil, Seq(v)))
              else
                eids.map((_, Seq(v), Nil))
              queryWire().updateNum(db, attrFull, attrType, data).call()
            } else {
              val data = if (nonEmpty)
                eids.map((_, Nil, Seq(value)))
              else
                eids.map((_, Seq(value), Nil))
              queryWire().updateStr(db, attrFull, attrType, enumPrefix, data).call()
            }
            save.foreach {
              case Right((t, tx, txInstant)) =>
                // Update client tx cells and arrays
                UpdateClientTx(
                  qr, valueArray, nsAlias, nsFull, attr, attrType, enums
                ).updateClient(
                  t, tx, txInstant,
                  tableRows, newVopt, oldVopt, valueColIndex,
                  affectedRows, affectedIndexes
                )

                if (nonEmpty)
                  println(s"Updated ${eids.length} entities: $attrFull value from " +
                    s"`$oldStr` to `$newStr`")
                else
                  println(s"Successfully retracted $attrFull value `$oldStr` " +
                    s"of ${eids.length} entities")

              case Left(err) =>
                editCellId = ""
                selectContent(groupedCell)
                if (nonEmpty)
                  window.alert(s"Error updating $attrFull value from " +
                    s"`$oldStr` to `$newStr`:\n$err")
                else
                  window.alert(s"Error retracting $attrFull value `$oldStr` " +
                    s"of ${eids.length} entities:\n$err")
                groupedCell.focus()
            }
          }
        }
      }
  }
}
