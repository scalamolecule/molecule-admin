package moleculeadmin.client.app.domain.query.data.groupedit
import java.io
import java.net.URI
import java.util.{Date, UUID}
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.Indexes
import moleculeadmin.client.app.domain.query.data.groupedit.ops.{GroupEditCodeException, _}
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.client.scalafiddle.ScalaFiddle
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.{Node, NodeList, document, window}
import rx.Ctx
import scalatags.JsDom.all._
import scala.collection.immutable
import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.util.matching


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
  extends RxBindings with ColOps with BodyElements with KeyEvents with TypeMappings {

  val Col(colIndex, _, _, _, _, attrType, _, card, _, enums, _, _, _, _) = col

  val colIndexes: Seq[Int] = columns.now.collect {
    case col if col.attrExpr != "edit" => col.colIndex
  }

  // Get input for scala right hand side code (before processing adds spinner!)
  val rhs: String = _html2str(document.getElementById(filterId).innerHTML).trim

  // Start spinner since compilation can take some seconds
  processing() = filterId

  val qr: QueryResult =
    queryCache.now.find(_.modelElements == modelElements.now).get.queryResult

  val tableRows: NodeList = document.getElementById("tableBody").childNodes

  val toTransferValueLambdas: Seq[Int => Any] = ToTransferValueLambdas(qr).get

  val updateCells: UpdateCells = UpdateCells(colIndex, attrType, card, tableRows)


  private def transformValues[ColType, TransferType](
    arrays: List[Array[Option[ColType]]],
    toColType: TransferType => ColType,
    updateCell: (Int, Option[ColType], Option[ColType]) => Unit
  ): Unit = {
    val scalaCode                = ScalaCode(col, rhs).get
    val scalaFiddle              = ScalaFiddle[TransferType](scalaCode)
    val arrayIndexes             = qr.arrayIndexes
    val origArray                = arrays(arrayIndexes(colIndex - 1))
    val editArray                = arrays(arrayIndexes(colIndex))
    var oldVopt                  = Option.empty[ColType]
    var newVopt                  = Option.empty[ColType]
    val tableRowIndexOffset      = offset.now
    val tableRowIndexMax         = curLastRow
    val sortCols                 = columns.now.filter(_.sortDir.nonEmpty)
    val unfiltered               = filters.now.isEmpty
    val (sortIndex, filterIndex) = Indexes(qr, sortCols, unfiltered).get
    val lastRow                  = actualRowCount
    var j                        = 0
    var tableRowIndex            = 0

    println(scalaCode)

    val indexBridge: Int => Int = {
      if (filterIndex.nonEmpty)
        (i: Int) => filterIndex(i)
      else if (sortIndex.nonEmpty)
        (i: Int) => sortIndex(i)
      else
        (i: Int) => i
    }

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

    // Minimize object creation for large edits
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

    CalculateGroupEdit(colIndexes, toTransferValueLambdas, scalaFiddle, lastRow, resolve)

    // Group edit completed - stop spinner
    processing() = ""
  }

  // Card one ------------------------------------------

  def string(): Unit = {
    val cellBaseClass                             = if (attrType == "BigInt" || attrType == "BigDecimal")
      "num" else "str"
    val toColType     : UndefOr[String] => String = attrType match {
      case "Date" => (s: js.UndefOr[String]) =>
        date2str(new Date(s.toOption.get.toLong))

      case _ => (s: js.UndefOr[String]) => s.toOption.get
    }
    val colValueToNode: String => Node            = attrType match {
      case "String" => (s: String) => _str2frags(s).render
      case _        => (s: String) => s.render
    }

    transformValues(
      qr.str,
      toColType,
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
    val colValueToItems = attrType match {
      case "String" => (vs: List[String]) => vs.sorted.map(v => li(_str2frags(v)))
      case _        => (vs: List[String]) => vs.sorted.map(li(_))
    }
    def transform[Ret](
      cellBaseClass: String,
      toColType: Ret => List[String]
    ): Unit = transformValues(
      qr.listStr,
      toColType,
      updateCells.cardMany(cellBaseClass, colValueToItems)
    )
    attrType match {
      case "String"  =>
        transform(
          if (enums.isEmpty) "items" else "str",
          (vs: js.Array[String]) => vs.toList.distinct
        )
      case "Boolean" => transform("str", (vs: js.Array[Boolean]) =>
        vs.toList.distinct.map(_.toString))

      case "Date" => transform("str", (vs: js.Array[js.Date]) =>
        vs.toList.distinct.map(jsDate => date2str(new Date(jsDate.getTime.toLong))))

      case "UUID" => transform("str", (vs: js.Array[UUID]) =>
        vs.toList.distinct.map(_.toString))
      case "URI"  => transform("str", (vs: js.Array[URI]) =>
        vs.toList.distinct.map(_.toString))
      case _      => transform("num", (vs: js.Array[String]) =>
        vs.toList.distinct)
    }
  }

  def listDouble(): Unit = {
    val toColType = attrType match {
      case "Int" => (vs: js.Array[Int]) => vs.toList.distinct.map(_.toDouble)
      case _     => (vs: js.Array[String]) => vs.toList.distinct.map(_.toDouble)
    }
    transformValues(
      qr.listNum,
      toColType,
      updateCells.cardMany("num", (vs: List[Double]) => vs.sorted.map(li(_)))
    )
  }


  // Card map ------------------------------------------

  def mapString(): Unit = {
    val colValueToItems = attrType match {
      case "String" => (vs: Map[String, String]) =>
        vs.toList.sortBy(_._1).map {
          case (k, v) => li(_str2frags(k + " -> " + v))
        }
      case _        => (vs: Map[String, String]) =>
        vs.toList.sortBy(_._1).map {
          case (k, v) => li(k + " -> " + v.toString)
        }
    }
    def transform[Ret](
      cellBaseClass: String,
      toColType: Ret => Map[String, String]
    ): Unit = transformValues(
      qr.mapStr,
      toColType,
      updateCells.cardMany(cellBaseClass, colValueToItems)
    )
    attrType match {
      case "String"  =>
        transform("items",
          (vs: js.Dictionary[String]) => vs.toMap)
      case "Boolean" =>
        transform("str",
          (vs: js.Dictionary[Boolean]) =>
            vs.toMap.map { case (k, v) => k -> v.toString })
      case "Date"    =>
        transform("str",
          (vs: js.Dictionary[js.Date]) =>
            vs.toMap.map {
              case (k, jsDate) => k -> date2str(new Date(jsDate.getTime().toLong))
            })
      case "UUID"    =>
        transform("str",
          (vs: js.Dictionary[UUID]) => vs.toMap.map { case (k, v) => k -> v.toString })
      case "URI"     =>
        transform("str",
          (vs: js.Dictionary[URI]) => vs.toMap.map { case (k, v) => k -> v.toString })
      case _         =>
        transform("str",
          (vs: js.Dictionary[String]) => vs.toMap)
    }
  }

  def mapDouble(): Unit = {
    val toColType       = attrType match {
      case "Int" =>
        (vs: js.Dictionary[Int]) =>
          vs.toMap.map { case (k, v) => k -> v.toDouble }
      case _     =>
        (vs: js.Dictionary[String]) =>
          vs.toMap.map { case (k, v) => k -> v.toDouble }
    }
    val colValueToItems =
      (vs: Map[String, Double]) =>
        vs.toList.sortBy(_._1).map { case (k, v) => li(k + " -> " + v) }

    transformValues(
      qr.mapNum,
      toColType,
      updateCells.cardMany("str", colValueToItems)
    )
  }
}
