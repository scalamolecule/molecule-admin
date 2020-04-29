package moleculeadmin.client.app.logic.query.data.groupEdit

import boopickle.Default._
import moleculeadmin.client.app.html.query.datatable.{BodyElements, HeadElements}
import moleculeadmin.client.app.logic.query.KeyEvents
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.Indexes
import moleculeadmin.client.app.logic.query.data.groupEdit.ops._
import moleculeadmin.client.scalafiddle.ScalaFiddle
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.{Node, NodeList, document, window}
import rx.Ctx
import scalatags.JsDom.all._
import util.client.rx.RxBindings
import scala.collection.immutable.Map
import scala.scalajs.js


/*
  Float is treated as Double to avoid precision problems from ScalaJS

  Long is treated as Double since it is opaque in ScalaJS
  see: https://stackoverflow.com/a/27823467/1211032

  To get precision correct, use BigDecimal and cast to Double (used for
  all number types except Int):
  floats.map(v => (v + BigDecimal(0.1)).toDouble)

  Mutable vars are used extensively here to minimize object creation
  during edits of large number of values
 */
case class GroupEdit(col: Col, filterId: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with ColOps
    with BodyElements with HeadElements
    with KeyEvents with TypeMappings {

  val Col(colIndex, _, _, nsFull, attr, attrType, _, card, opt, enums, _, _, _, _, _) = col

  val colIndexes: Seq[Int] = columns.now.collect {
    case col if col.attrExpr != "edit" => col.colIndex
  }

  // Scala expression to be applied
  val editExpr: String = _html2str(document.getElementById(filterId).innerHTML).trim

  // Start spinner since compilation can take some seconds
  processing() = filterId

  val qr: QueryResult = queryCache.queryResult

  val tableRows: NodeList = document.getElementById("tableBody").childNodes

  val colType2StringLambdas: Seq[Int => Any] = ColType2TransferTypeLambdas(qr).get

  val updateCells: UpdateCells = UpdateCells(colIndex, attrType, card, tableRows)


  private def transformValues[ColType, TransferType](
    arrays: List[Array[Option[ColType]]],
    toColType: TransferType => ColType,
    updateCell: (Int, Option[ColType], Option[ColType]) => Unit
  ): Unit = {
    val scalaCode           = ScalaCode(columns.now, col, editExpr).get
    val scalaFiddle         = ScalaFiddle[TransferType](scalaCode)
    val arrayIndexes        = qr.arrayIndexes
    val origArray           = arrays(arrayIndexes(colIndex - 1))
    val editArray           = arrays(arrayIndexes(colIndex))
    var oldVopt             = Option.empty[ColType]
    var newVopt             = Option.empty[ColType]
    val tableRowIndexOffset = offset.now
    val tableRowIndexMax    = curLastRow
    val sortCols            = columns.now.filter(_.sortDir.nonEmpty)
    val unfiltered          = filters.now.isEmpty
    val indexBridge         = Indexes(qr, sortCols, unfiltered).getIndexBridge
    val lastRow             = actualRowCount
    var j                   = 0
    var tableRowIndex       = 0

    //    println(scalaCode)

    def alert(error: String): Nothing = {
      println(scalaCode)
      window.alert(error + " - see console for details")
      throw new GroupEditCodeException(error)
    }

    def updateClient(i: Int): Unit = {
      if (i >= tableRowIndexOffset && i < tableRowIndexMax) {
        updateCell(tableRowIndex, oldVopt, newVopt)
        tableRowIndex += 1
      }
    }

    val resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit = {
      if (card == 1) {
        (i: Int, toTransferType: Int => js.Tuple2[TransferType, String]) => {
          j = indexBridge(i)
          oldVopt = origArray(j)
          newVopt = toTransferType(j) match {
            case js.Tuple2(v, "")
              if v.asInstanceOf[js.UndefOr[_]].isEmpty => None
            case js.Tuple2(v, "")                      => Some(toColType(v))
            case js.Tuple2(_, error)                   => alert(error)
          }
          updateClient(i)
          editArray(j) = newVopt
        }
      } else {
        (i: Int, toTransferType: Int => js.Tuple2[TransferType, String]) => {
          j = indexBridge(i)
          oldVopt = origArray(j)
          newVopt = toTransferType(j) match {
            case js.Tuple2(Nil, "")  => None
            case js.Tuple2(vs, "")   => Some(toColType(vs))
            case js.Tuple2(_, error) => alert(error)
          }
          updateClient(i)
          editArray(j) = newVopt
        }
      }
    }

    // Insert/update used edit expression
    EditExprs(col).upsert(editExpr)

    ProcessGroupEdit(colIndexes, colType2StringLambdas, scalaFiddle, lastRow, resolve)

    // Group edit completed - stop spinner
    processing() = ""
  }


  // Card one ------------------------------------------

  def string(): Unit = {
    val cellBaseClass = if (attrType == "BigInt" || attrType == "BigDecimal")
      "num" else "str"

    val colValueToNode: String => Node = attrType match {
      case "String" => (s: String) => _str2frags(s).render
      case _        => (s: String) => s.render
    }

    transformValues(
      qr.str,
      (s: js.UndefOr[String]) => s.toOption.get,
      updateCells.cardOne(cellBaseClass, colValueToNode),
    )
  }

  def double(): Unit = {
    transformValues(
      qr.num,
      (s: js.UndefOr[String]) => s.toOption.get.toDouble,
      updateCells.cardOne("num", (v: Double) => v.render),
    )
  }


  // Card many ------------------------------------------

  def listString(): Unit = {
    val cellBaseClass   = attrType match {
      case "String" if enums.isEmpty                      => "items"
      case "String" | "Boolean" | "Date" | "UUID" | "URI" => "str"
      case "BigInt" | "BigDecimal"                        => "num"
    }
    val colValueToItems = attrType match {
      case "String" => (vs: List[String]) => vs.sorted.map(v => li(_str2frags(v)))
      case _        => (vs: List[String]) => vs.sorted.map(li(_))
    }
    transformValues(
      qr.listStr,
      (vs: js.Array[String]) => vs.toList.distinct,
      updateCells.cardMany(cellBaseClass, colValueToItems)
    )
  }

  def listDouble(): Unit = {
    transformValues(
      qr.listNum,
      (vs: js.Array[String]) => vs.toList.distinct.map(_.toDouble),
      updateCells.cardMany("num", (vs: List[Double]) => vs.sorted.map(li(_)))
    )
  }


  // Card map ------------------------------------------

  def mapString(): Unit = {
    val cellBaseClass   = if (attrType == "String") "items" else "str"
    val colValueToItems = attrType match {
      case "String" => (vs: Map[String, String]) =>
        vs.toList.sortBy(_._1).map {
          case (k, v) => li(_str2frags(k + " -> " + v))
        }
      case _        => (vs: Map[String, String]) =>
        vs.toList.sortBy(_._1).map {
          case (k, v) => li(k + " -> " + v)
        }
    }
    transformValues(
      qr.mapStr,
      (vs: js.Dictionary[String]) => vs.toMap,
      updateCells.cardMany(cellBaseClass, colValueToItems)
    )
  }

  def mapDouble(): Unit = {
    val colValueToItems =
      (vs: Map[String, Double]) =>
        vs.toList.sortBy(_._1).map { case (k, v) => li(k + " -> " + v) }

    transformValues(
      qr.mapNum,
      (vs: js.Dictionary[String]) => vs.toMap.map { case (k, v) => k -> v.toDouble },
      updateCells.cardMany("str", colValueToItems)
    )
  }
}
