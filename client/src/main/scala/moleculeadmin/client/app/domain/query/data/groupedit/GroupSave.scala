package moleculeadmin.client.app.domain.query.data.groupedit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.Indexes
import moleculeadmin.client.app.domain.query.keyEvents.Paging
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{LI, TableCell, TableRow}
import org.scalajs.dom.{Node, NodeList, document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalatags.JsDom.all._

case class GroupSave(col: Col)(implicit val ctx: Ctx.Owner)
  extends RxBindings with ColOps with BodyElements
//    with KeyEvents
  with Paging
{
  type keepBooPickleImport_GroupSave = PickleState

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType,
  _, _, enums, _, _, _, _, _) = col

  val attrFull   = s":$nsFull/${clean(attr)}"
  val enumPrefix = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"
  val filterId   = "filter-" + colIndex

  // Start spinner since saving to db can take time
  processing() = filterId

  val cache = queryCache
  val qr    = cache.queryResult

  val eidIndex = getEidColIndex(columns.now, colIndex, nsAlias, nsFull)
  val eidArray = qr.num(eidIndex)
  var eid      = 0L

  val origIndex = qr.arrayIndexes(colIndex - 1)
  val editIndex = qr.arrayIndexes(colIndex)

  val tableRows           = document.getElementById("tableBody").childNodes
  var tableRowIndexOffset = offset.now
  var tableRowIndexMax    = curLastRow

  val sortCols    = columns.now.filter(_.sortDir.nonEmpty)
  val unfiltered  = filters.now.isEmpty
  val indexBridge = Indexes(qr, sortCols, unfiltered).getIndexBridge
  val lastRow     = actualRowCount

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
        newVopt.foreach { newV =>
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


  /**
   * @tparam T String / Double
   **/
  def saveData[ColType, T](
    arrays: List[Array[Option[ColType]]],
    prepare: (Long, Option[ColType], Option[ColType]) => Seq[(Long, Seq[T], Seq[T])],
    save: Seq[(Long, Seq[T], Seq[T])] => Future[Either[String, (Long, Long, String)]],
    updateCell: (Int, Option[ColType]) => Unit
  ): Unit = {
    val origArray = arrays(origIndex)
    val editArray = arrays(editIndex)
    var oldVopt   = Option.empty[ColType]
    var newVopt   = Option.empty[ColType]
    val data      = new ListBuffer[(Long, Seq[T], Seq[T])]
    var i         = 0
    var j         = 0
    while (i < lastRow) {
      j = indexBridge(i)
      eid = eidArray(j).get.toLong
      oldVopt = origArray(j)
      newVopt = editArray(j)
      data ++= prepare(eid, oldVopt, newVopt)
      i += 1
    }

    if (data.nonEmpty) {
      save(data).map {
        case Right((t, tx, txInstant)) =>
          println(s"Successfully saved ${data.length} changes for attr `$attrFull`")
          i = 0
          var tableRowIndex = 0
          while (i < lastRow) {
            j = indexBridge(i)
            newVopt = editArray(j)
            origArray(j) = newVopt
            editArray(j) = newVopt
            if (i >= tableRowIndexOffset && i < tableRowIndexMax) {
              updateCell(tableRowIndex, newVopt)
              tableRowIndex += 1
            }
            i += 1
          }

          // Invalidate previous caches to avoid old attr data to hang over
          queryCache = cache

          // Turn spinner off
          processing() = ""

        case Left(err) =>
          window.alert(s"Error saving $attrFull changes:\n$err")
      }
    } else {
      println("No changes")
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
      queryWire().updateStr(db, attrFull, attrType, enumPrefix, data).call()

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
      queryWire().updateNum(db, attrFull, attrType, data).call()

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
      queryWire().updateStr(db, attrFull, attrType, enumPrefix, data).call()

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
      queryWire().updateNum(db, attrFull, attrType, data).call()

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
      queryWire().updateStr(db, attrFull, "String", "", data).call()

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
      queryWire().updateStr(db, attrFull, "String", "", data).call()

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
