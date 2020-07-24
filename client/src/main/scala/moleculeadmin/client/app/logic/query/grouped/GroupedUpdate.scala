package moleculeadmin.client.app.logic.query.grouped

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.html.query.GroupedAttrElements
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.TypeValidation
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.Col
import org.scalajs.dom.html.TableCell
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{NodeList, document, window}
import rx.Ctx
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


abstract class GroupedUpdate[T](col: Col)(implicit ctx: Ctx.Owner)
  extends GroupedData[T](col) with GroupedAttrElements with TypeValidation {

  val attrFull = s":$nsFull/${clean(attr)}"
  val rowId    = (rowIndex: Int) => s"grouped-row-$colIndex-$rowIndex"
  val cellId   = (rowIndex: Int) => s"grouped-cell-$colIndex-$rowIndex"


  def updateLambda(tableRows: NodeList): Int => () => Unit = {
    rowIndex: Int =>
      () => {
        val groupedCell     = document.getElementById(cellId(rowIndex))
          .asInstanceOf[HTMLInputElement]
        val oldSopt         = groupedData(rowIndex)._1.map(t => _html2str(t))
        val (newSopt, newS) = groupedCell.innerHTML.trim match {
          case "" => (Option.empty[String], "")
          case s0 => val s = _html2str(s0); (Some(s), s)
        }

        if (editCellId.nonEmpty
          && editCellId == groupedCell.id
          && oldSopt != newSopt
        ) {
          if (attrType != "String" && newS.contains('\n')) {
            editCellId = ""
            window.alert(
              s"Can't save multiple $attrFull values of type `$attrType`:\n$newS"
            )
            groupedCell.focus()

          } else if (enums.nonEmpty && !enums.contains(newS)) {
            editCellId = ""
            window.alert(
              s"Can't save non-defined $attrFull enum value `$newS`" +
                "\nThe following enum values are defined:\n  " +
                enums.mkString("\n  ")
            )
            groupedCell.focus()

          } else if (newSopt.nonEmpty && !validGroupedValue(newS)) {
            editCellId = ""
            val msg = if (attrType == "ref")
              s"Invalid card-$card ref id(s) for $attrFull :\n$newS"
            else
              s"Invalid card-$card value of type `$attrType` " +
                s"for attribute $attrFull :\n$newS"
            window.alert(msg)
            groupedCell.focus()

          } else {
            processUpdate(tableRows, groupedCell, oldSopt, newSopt, rowIndex)
          }
        }
      }
  }

  protected def validGroupedValue(s: String): Boolean = {
    card match {
      case 1 | 2 => valid(attrType, s)
      case 3     => s match {
        case r"(.+?)$k *-> *(.+)$v" => valid(attrType, v)
        case _                      => false
      }
    }
  }

  def processUpdate(
    tableRows: NodeList,
    groupedCell: HTMLInputElement,
    oldSopt: Option[String],
    newSopt: Option[String],
    rowIndex: Int
  ): Unit = {
    val indexBridge      = cachedIndexBridge
    val filteredRowCount = actualRowCount
    val eidIndex         = getEidColIndex(columns.now, colIndex, nsAlias, nsFull)
    val eidArray         = qr.num(qr.arrayIndexes(eidIndex))
    var eids             = List.empty[Long]
    var i                = 0
    var j                = 0

    // Affected rows (for card-one clientUpdate only)
    val positives = new Array[Int](filteredRowCount)
    var posIndex  = 0

    colType match {
      case "string" =>
        while (i < filteredRowCount) {
          j = indexBridge(i)
          if (valueArray(j) == oldSopt) {
            eids = eidArray(j).get.toLong :: eids
            positives(posIndex) = j
            posIndex += 1
            // Update internal cache (only for card-one)
            valueArray(j) = newSopt.asInstanceOf[Option[T]]
          }
          i += 1
        }

      case "double" =>
        val oldVopt = oldSopt.map(_.toDouble)
        while (i < filteredRowCount) {
          j = indexBridge(i)
          if (valueArray(j) == oldVopt) {
            eids = eidArray(j).get.toLong :: eids
            positives(posIndex) = j
            posIndex += 1
            // Update internal cache (only for card-one)
            valueArray(j) = newSopt.map(_.toDouble).asInstanceOf[Option[T]]
          }
          i += 1
        }

      case "listString" =>
        oldSopt match {
          case None       =>
            while (i < filteredRowCount) {
              j = indexBridge(i)
              if (valueArray(j).isEmpty)
                eids = eidArray(j).get.toLong :: eids
              i += 1
            }
          case Some(oldS) =>
            while (i < filteredRowCount) {
              j = indexBridge(i)
              val oldValues = valueArray(j).asInstanceOf[Option[List[String]]]
              if (oldValues.exists(_.contains(oldS))) {
                eids = eidArray(j).get.toLong :: eids
              }
              i += 1
            }
        }

      case "listDouble" =>
        oldSopt match {
          case None       =>
            while (i < filteredRowCount) {
              j = indexBridge(i)
              if (valueArray(j).isEmpty)
                eids = eidArray(j).get.toLong :: eids
              i += 1
            }
          case Some(oldS) =>
            val oldV = oldS.toDouble
            while (i < filteredRowCount) {
              j = indexBridge(i)
              val oldValues = valueArray(j).asInstanceOf[Option[List[Double]]]
              if (oldValues.exists(_.contains(oldV))) {
                eids = eidArray(j).get.toLong :: eids
              }
              i += 1
            }
        }

      case "mapString" =>
        oldSopt match {
          case None       =>
            while (i < filteredRowCount) {
              j = indexBridge(i)
              if (valueArray(j).isEmpty)
                eids = eidArray(j).get.toLong :: eids
              i += 1
            }
          case Some(oldS) =>
            val (k, v) = oldS match {
              // Greedy match to split by first arrow
              case r"(.+?)$k *-> *(.+)$v" => (k, v)
            }
            while (i < filteredRowCount) {
              j = indexBridge(i)
              val oldValues = valueArray(j).asInstanceOf[Option[Map[String, String]]]
              if (oldValues.exists(pairs => pairs.contains(k) && pairs(k) == v)) {
                eids = eidArray(j).get.toLong :: eids
              }
              i += 1
            }
        }

      case "mapDouble" =>
        oldSopt match {
          case None       =>
            while (i < filteredRowCount) {
              j = indexBridge(i)
              if (valueArray(j).isEmpty)
                eids = eidArray(j).get.toLong :: eids
              i += 1
            }
          case Some(oldS) =>
            val (k, v) = oldS match {
              case r"(.+?)$k *-> *(.+)$v" => (k, v.toDouble)
            }
            while (i < filteredRowCount) {
              j = indexBridge(i)
              val oldValues = valueArray(j).asInstanceOf[Option[Map[String, Double]]]
              if (oldValues.exists(pairs => pairs.contains(k) && pairs(k) == v)) {
                eids = eidArray(j).get.toLong :: eids
              }
              i += 1
            }
        }
    }


    // Update db call
    val saveLambdas = colType match {
      case "string" | "listString" =>
        val enumPrefix  = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"
        val retractions = oldSopt.fold(Seq.empty[String])(v => Seq(v))
        val assertions  = newSopt.fold(Seq.empty[String])(v => Seq(v))
        val data        = eids.map((_, retractions, assertions))
        queryWireAjax().updateStr(db, attrFull, attrType, enumPrefix, data).call()

      case "double" | "listDouble" =>
        val retractions = oldSopt.fold(Seq.empty[Double])(v => Seq(v.toDouble))
        val assertions  = newSopt.fold(Seq.empty[Double])(v => Seq(v.toDouble))
        val data        = eids.map((_, retractions, assertions))
        queryWireAjax().updateNum(db, attrFull, attrType, data).call()

      case maps =>
        val retractions = oldSopt.fold(Seq.empty[String]) { pair =>
          val k = pair.substring(0, pair.indexOf("->")).trim
          val v = pair.substring(pair.indexOf("->") + 2).trim
          Seq(s"$k@$v")
        }
        val assertions  = newSopt.fold(Seq.empty[String]) { pair =>
          val k = pair.substring(0, pair.indexOf("->")).trim
          val v = pair.substring(pair.indexOf("->") + 2).trim
          Seq(s"$k@$v")
        }
        val data        = eids.map((_, retractions, assertions))
        queryWireAjax().updateStr(db, attrFull, "String", "", data).call()
    }

    // call db
    saveLambdas.foreach {
      case Right((t, tx, txInstant)) =>
        val valueFiltering = filters.now.exists(_._1 == colIndex)
        card match {
          case 1 if !valueFiltering =>
            // Allow in-memory replacing if not already filtering
            updateClient(t, tx, txInstant)
          case _                    =>
            // Need to re-load since
            // - Some filters
            // - there's no single `newTopt` card-many value
            modelElements.recalc()
        }

        if (newSopt.nonEmpty)
          println(s"Updated ${eids.length} entities: $attrFull value from " +
            s"`$oldSopt` to `$newSopt`")
        else
          println(s"Successfully retracted $attrFull value `$oldSopt` " +
            s"of ${eids.length} entities")

      case Left(err) =>
        editCellId = ""
        selectContent(groupedCell)
        if (newSopt.nonEmpty)
          window.alert(s"Error updating $attrFull value from " +
            s"`$oldSopt` to `$newSopt`:\n$err")
        else
          window.alert(s"Error retracting $attrFull value `$oldSopt` " +
            s"of ${eids.length} entities:\n$err")
        groupedCell.focus()
    }


    def updateClient(t: Long, tx: Long, txInstant: String) = {
      // Update edit cell in grouped data table
      groupedCell.innerHTML = ""
      if (newSopt.nonEmpty) {
        if (attrType == "String") {
          groupedCell.appendChild(_str2frags(newSopt.get).render)
        } else {
          groupedCell.appendChild(newSopt.get.render)
        }
      } else {
        groupedCell.appendChild("".render)
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

      var affectedRows    = List.empty[Int]
      val valueColIndex   = colIndex + 1
      val oldTableStr     = oldSopt.getOrElse("")
      val newTableStr     = newSopt.getOrElse("")
      var i               = 0
      val tableRowsLength = tableRows.length
      while (i < tableRowsLength) {
        val tableCell  = tableRows.item(i).childNodes
          .item(valueColIndex).asInstanceOf[TableCell]
        val tableCellV = html2value(tableCell.innerHTML)
        if (tableCellV == oldTableStr) {
          affectedRows = affectedRows :+ i
          tableCell.innerHTML = ""
          if (newSopt.nonEmpty)
            tableCell.appendChild(value2node(newTableStr))
        }
        i += 1
      }

      val affectedIndexes = new Array[Int](posIndex)
      System.arraycopy(positives, 0, affectedIndexes, 0, posIndex)

      val newTopt = (if (colType == "string")
        newSopt else newSopt.map(_.toDouble)
        ).asInstanceOf[Option[T]]
      // Update client tx cells and arrays
      GroupedUpdateClient(
        qr, valueArray, nsAlias, nsFull, attr, attrType, enums
      ).updateClient(
        t, tx, txInstant,
        tableRows,
        newTopt,
        valueColIndex,
        affectedRows, affectedIndexes
      )

      // Focus next grouped value in refreshed grouped data table
      val nextRowIndex = if (rowIndex == groupedData.length - 1)
        rowIndex else rowIndex + 1
      document.getElementById(s"grouped-cell-$colIndex-$nextRowIndex")
        .asInstanceOf[HTMLInputElement].focus()
    }
  }
}
