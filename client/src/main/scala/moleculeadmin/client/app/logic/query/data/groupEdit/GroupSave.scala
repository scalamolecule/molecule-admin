package moleculeadmin.client.app.logic.query.data.groupEdit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.html.query.datatable.BodyElements
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.keyEvents.Paging
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{LI, TableCell, TableRow}
import org.scalajs.dom.{Node, NodeList, document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import util.client.rx.RxBindings
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class GroupSave(col: Col)(implicit val ctx: Ctx.Owner)
  extends RxBindings with ColOps with BodyElements with Paging {

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType,
  _, _, enums, _, _, _, _, _, _) = col

  val attrFull            = s":$nsFull/${clean(attr)}"
  val enumPrefix          = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"
  val filterId            = "filter-" + colIndex
  val qr                  = cachedQueryResult
  val indexBridge         = cachedIndexBridge
  val eidIndex            = getEidColIndex(columns.now, colIndex, nsAlias, nsFull)
  val eidArray            = qr.num(qr.arrayIndexes(eidIndex))
  var eid                 = 0L
  val origIndex           = qr.arrayIndexes(colIndex - 1)
  val editIndex           = qr.arrayIndexes(colIndex)
  val tableRows           = document.getElementById("tableBody").childNodes
  var tableRowIndexOffset = offset.now
  var tableRowIndexMax    = curLastRow
  val lastRow             = actualRowCount

  case class CellUpdater[ColType](cellBaseClass: String) {
    var cells   : NodeList  = null
    var origCell: TableCell = null
    var editCell: TableCell = null
    val origColIndex        = colIndex
    val editColIndex        = colIndex + 1

    def updateCell(
      colValueToNode: ColType => Node,
      colValueToNodeOrig: Option[ColType => Node] = None,
    ): (Int, Option[ColType]) => Unit = {
      (
        tableRowIndex: Int,
        newVopt: Option[ColType]
      ) => {
        cells = tableRows.item(tableRowIndex).childNodes
        origCell = cells.item(origColIndex).asInstanceOf[TableCell]
        editCell = cells.item(editColIndex).asInstanceOf[TableCell]
        origCell.innerHTML = ""
        editCell.innerHTML = ""
        newVopt.fold(
          // turn of retracted
          editCell.className = cellBaseClass
        ) { newV =>
          origCell.appendChild(
            colValueToNodeOrig.fold(colValueToNode(newV))(_ (newV))
          )
          editCell.className = cellBaseClass
          editCell.appendChild(colValueToNode(newV))
        }
      }
    }

    def cardOne(colValueToNode: ColType => Node)
    : (Int, Option[ColType]) => Unit = {
      updateCell((newV: ColType) => colValueToNode(newV))
    }

    def cardMany(colValueToItems: ColType => Seq[TypedTag[LI]])
    : (Int, Option[ColType]) => Unit = {
      updateCell((newV: ColType) => ul(colValueToItems(newV)).render)
    }

    def cardMap(
      colValueToItemsEdit: ColType => Seq[TypedTag[LI]],
      colValueToItemsOrig: ColType => Seq[TypedTag[TableRow]],
    ): (Int, Option[ColType]) => Unit = {
      updateCell(
        (newV: ColType) => ul(colValueToItemsEdit(newV)).render,
        Some(
          (newV: ColType) => table(cls := "mapPairs",
            colValueToItemsOrig(newV)
          ).render
        )
      )
    }
  }

  def save(): Unit = {
    colType match {
      case "string"     => string()
      case "double"     => double()
      case "listString" => listString()
      case "listDouble" => listDouble()
      case "mapString"  => mapString()
      case "mapDouble"  => mapDouble()
    }
  }

  /**
   * @tparam T String / Double
   **/
  def saveData[ColType, T](
    arrays: List[Array[Option[ColType]]],
    prepare: (Long, Option[ColType], Option[ColType]) => Seq[(Long, Seq[T], Seq[T])],
    save: Seq[(Long, Seq[T], Seq[T])] => Future[Either[String, (Long, Long, String)]],
    updateCell: (Int, Option[ColType]) => Unit
  ): Unit = {
    val origArray       = arrays(origIndex)
    val editArray       = arrays(editIndex)
    val origArrayCached = origArray.clone()
    var oldVopt         = Option.empty[ColType]
    var newVopt         = Option.empty[ColType]
    val data            = new ListBuffer[(Long, Seq[T], Seq[T])]
    var i               = 0
    var j               = 0
    var tableRowIndex   = 0
    var clientUpdated   = false

    def rollback(): Unit = {
      i = 0
      while (i < lastRow) {
        j = indexBridge(i)
        // Reset old orig value
        origArray(j) = origArrayCached(j)
        i += 1
      }
      // Redraw table body/footer of current page
      offset.recalc()
    }

    try {
      if (filters.now.nonEmpty && lastRow != cachedSortFilterIndex.length) {
        val err = "Unexpected internal error: " +
          s"lastRow count ($lastRow) doesn't match " +
          s"cachedFilterIndex.length (${cachedSortFilterIndex.length})"
        window.alert(err)
        throw new RuntimeException(err)
      }

      // Update client
      while (i < lastRow) {
        j = indexBridge(i)

        // Current values
        eid = eidArray(j).get.toLong
        oldVopt = origArray(j)
        newVopt = editArray(j)

        // Collect db update stmts
        data ++= prepare(eid, oldVopt, newVopt)

        // Update table cells
        if (i >= tableRowIndexOffset && i < tableRowIndexMax) {
          updateCell(tableRowIndex, newVopt)
          tableRowIndex += 1
        }

        // Update client cache of original column
        origArray(j) = newVopt

        i += 1
      }
      clientUpdated = true

      if (data.nonEmpty) {
        save(data.toSeq).map {
          case Right(_) =>
            println(s"Successfully saved ${data.length} changes for attr `$attrFull`")

            // Reset cache to avoid old attr data to hang over
            cachedQueryResult = qr

          case Left(err) =>
            rollback()
            val msg = s"Couldn't save new group edit data for attr `$attrFull` to database." +
              s"\nSuccessfully rolling back data in browser. " +
              s"\nError was:\n" + err
            println(msg)
            window.alert(msg)
        }
      } else {
        println("No changes")
      }
    } catch {
      case e: Throwable =>
        val err = if (clientUpdated) {
          rollback()
          s"Successfully rolled back from unexpected group edit error for attr `$attrFull`: " + e
        } else {
          s"Unexpected error saving new group edit data for attr `$attrFull`: " + e
        }
        println(err)
        window.alert(err)
        throw e
    }
  }


  def string(): Unit = {
    val prepare: (Long, Option[String], Option[String]) =>
      Seq[(Long, Seq[String], Seq[String])]
    = (eid: Long, oldVopt: Option[String], newVopt: Option[String]) => {
      if (oldVopt == newVopt) {
        Nil
      } else if (newVopt.nonEmpty) {
        Seq((eid, Nil, Seq(newVopt.get)))
      } else {
        Seq((eid, Seq(oldVopt.get), Nil))
      }
    }

    val save = (data: Seq[(Long, Seq[String], Seq[String])]) =>
      queryWireAjax().updateStr(db, attrFull, attrType, enumPrefix, data).call()

    val cellUpdater = CellUpdater[String]("str")
    val updateCell  = attrType match {
      case "String" => cellUpdater.cardOne((v: String) => _str2frags(v).render)
      case _        => cellUpdater.cardOne((v: String) => v.render)
    }

    saveData(qr.str, prepare, save, updateCell)
  }


  def double(): Unit = {
    val prepare: (Long, Option[Double], Option[Double]) =>
      Seq[(Long, Seq[Double], Seq[Double])]
    = (eid: Long, oldVopt: Option[Double], newVopt: Option[Double]) => {
      if (oldVopt == newVopt) {
        Nil
      } else if (newVopt.nonEmpty) {
        Seq((eid, Nil, Seq(newVopt.get)))
      } else {
        Seq((eid, Seq(oldVopt.get), Nil))
      }
    }

    val save = (data: Seq[(Long, Seq[Double], Seq[Double])]) =>
      queryWireAjax().updateNum(db, attrFull, attrType, data).call()

    val updateCell = CellUpdater[Double]("num").cardOne((v: Double) => v.render)

    saveData(qr.num, prepare, save, updateCell)
  }


  def listString(): Unit = {
    val prepare: (
      Long,
        Option[List[String]],
        Option[List[String]]
      ) =>
      Seq[(Long, Seq[String], Seq[String])]
    = (
      eid: Long,
      oldVopt: Option[List[String]],
      newVopt: Option[List[String]]
    ) => {
      val oldSet = oldVopt.fold(Set.empty[String])(_.toSet)
      val newSet = newVopt.fold(Set.empty[String])(_.toSet)
      if (oldSet == newSet) {
        Nil
      } else {
        val (retracts, asserts) = (oldSet.diff(newSet), newSet.diff(oldSet))
        Seq((eid, retracts.toSeq, asserts.toSeq))
      }
    }

    val save = (data: Seq[(Long, Seq[String], Seq[String])]) =>
      queryWireAjax().updateStr(db, attrFull, attrType, enumPrefix, data).call()

    val updateCell = attrType match {
      case "String" =>
        CellUpdater[List[String]]("items")
          .cardMany((vs: List[String]) => vs.sorted.map(v => li(_str2frags(v))))

      case "Boolean" | "Date" | "UUID" | "URI" =>
        CellUpdater[List[String]]("str")
          .cardMany((vs: List[String]) => vs.sorted.map(v => li(v)))

      case "BigInt" | "BigDecimal" =>
        CellUpdater[List[String]]("num")
          .cardMany((vs: List[String]) => vs.sorted.map(v => li(v)))
    }

    saveData(qr.listStr, prepare, save, updateCell)
  }


  def listDouble(): Unit = {
    val prepare: (
      Long,
        Option[List[Double]],
        Option[List[Double]]
      ) =>
      Seq[(Long, Seq[Double], Seq[Double])]
    = (eid: Long, oldVopt: Option[List[Double]], newVopt: Option[List[Double]]) => {
      val oldSet = oldVopt.fold(Set.empty[Double])(_.toSet)
      val newSet = newVopt.fold(Set.empty[Double])(_.toSet)
      if (oldSet == newSet) {
        Nil
      } else {
        val (retracts, asserts) = (oldSet.diff(newSet), newSet.diff(oldSet))
        Seq((eid, retracts.toSeq, asserts.toSeq))
      }
    }

    val save = (data: Seq[(Long, Seq[Double], Seq[Double])]) =>
      queryWireAjax().updateNum(db, attrFull, attrType, data).call()

    val cellUpdater = CellUpdater[List[Double]]("num")
    val updateCell  = attrType match {
      case "datom" | "ref" =>
        cellUpdater.cardMany((vs: List[Double]) =>
          vs.map(_.toLong).sorted.map(ref =>
            li(
              cls := Rx(if (ref == curEntity()) "eidChosen" else "eid"),
              ref,
              onmouseover := { () => curEntity() = ref }
            )
          )
        )
      case _               =>
        cellUpdater.cardMany((vs: List[Double]) => vs.sorted.map(li(_)))
    }

    saveData(qr.listNum, prepare, save, updateCell)
  }

  private def conflate[T](m: Map[String, T]): Set[String] =
    m.map { case (k, v) => k + "@" + v }.toSet


  def mapString(): Unit = {
    val prepare: (
      Long,
        Option[Map[String, String]],
        Option[Map[String, String]]
      ) =>
      Seq[(Long, Seq[String], Seq[String])]
    = (
      eid: Long,
      oldVopt: Option[Map[String, String]],
      newVopt: Option[Map[String, String]]
    ) => {
      val oldSet = oldVopt.fold(Set.empty[String])(conflate)
      val newSet = newVopt.fold(Set.empty[String])(conflate)
      if (oldSet == newSet) {
        Nil
      } else {
        val (retracts, asserts) = (oldSet.diff(newSet), newSet.diff(oldSet))
        Seq((eid, retracts.toSeq, asserts.toSeq))
      }
    }

    // Key-prefixed mapStr attribute values are all of type String
    val save = (data: Seq[(Long, Seq[String], Seq[String])]) =>
      queryWireAjax().updateStr(db, attrFull, "String", "", data).call()

    val updateCell = attrType match {
      case "String" =>
        CellUpdater[Map[String, String]]("items")
          .cardMap(
            (pairs: Map[String, String]) =>
              pairs.toList.sortBy(_._1).map {
                case (k, v) => li(_str2frags(k + " -> " + v))
              },
            (pairs: Map[String, String]) =>
              pairs.toSeq.sortBy(_._1).map {
                case (k, v) => tr(td(k), td("➜"), _str2frags(v))
              }
          )

      case _ =>
        CellUpdater[Map[String, String]]("str")
          .cardMap(
            (pairs: Map[String, String]) =>
              pairs.toList.sortBy(_._1).map {
                case (k, v) => li(k + " -> " + v)
              },
            (pairs: Map[String, String]) =>
              pairs.toSeq.sortBy(_._1).map {
                case (k, v) => tr(td(k), td("➜"), v)
              }
          )
    }

    saveData(qr.mapStr, prepare, save, updateCell)
  }


  def mapDouble(): Unit = {
    val prepare: (
      Long,
        Option[Map[String, Double]],
        Option[Map[String, Double]]
      ) =>
      Seq[(Long, Seq[String], Seq[String])]
    = (
      eid: Long,
      oldVopt: Option[Map[String, Double]],
      newVopt: Option[Map[String, Double]]
    ) => {
      val oldSet = oldVopt.fold(Set.empty[String])(conflate)
      val newSet = newVopt.fold(Set.empty[String])(conflate)
      if (oldSet == newSet) {
        Nil
      } else {
        val (retracts, asserts) = (oldSet.diff(newSet), newSet.diff(oldSet))
        Seq((eid, retracts.toSeq, asserts.toSeq))
      }
    }

    // Key-prefixed mapNum attribute values are all of type String
    val save = (data: Seq[(Long, Seq[String], Seq[String])]) =>
      queryWireAjax().updateStr(db, attrFull, "String", "", data).call()

    val updateCell =
      CellUpdater[Map[String, Double]]("str")
        .cardMap(
          (pairs: Map[String, Double]) =>
            pairs.toSeq.sortBy(_._1).map {
              case (k, v) => li(k + " -> " + v)
            },
          (pairs: Map[String, Double]) =>
            pairs.toSeq.sortBy(_._1).map {
              case (k, v) => tr(td(k), td("➜"), v)
            }
        )
    saveData(qr.mapNum, prepare, save, updateCell)
  }
}
