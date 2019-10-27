package moleculeadmin.client.app.domain.query.data.groupedit
import java.net.URI
import java.util.{Date, UUID}
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.Indexes
import moleculeadmin.client.app.domain.query.data.groupedit.ops.{AttrLambdas, AttrTokens, CalculateGroupEdit, ScalaCode, UpdateCells}
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.client.scalafiddle.ScalafiddleApi
import moleculeadmin.shared.ast.query
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{LI, TableCell}
import org.scalajs.dom.{Document, Node, NodeList, document}
import rx.Ctx
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

/*
  Float is treated as Double to avoid precision problems from ScalaJS

  Long is treated as Double since it is opaque in ScalaJS
  see: https://stackoverflow.com/a/27823467/1211032


  To get precision correct, use BigDecimal and cast to Double (used for
  all number types except Int):
  floats.map(v => (v + BigDecimal(0.1)).toDouble)
 */
case class GroupEdit(col: Col, filterId: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with ColOps with BodyElements with KeyEvents with AttrTokens {

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType, card, _, enums,
  aggrType, expr, sortDir, sortPos) = col


  // Build scala code elements for scalafiddle

  val lhsTypesProcess       = new ListBuffer[String]
  val lhsParamsTypesProcess = new ListBuffer[String]
  val lhsTypes              = new ListBuffer[String]
  val lhsParamsTypes        = new ListBuffer[String]
  val lhsParams             = new ListBuffer[String]
  val colTypes              = new ListBuffer[String]
  val colIndexes            = new ListBuffer[Int]

  columns.now.collect {
    case Col(colIndex, _, `nsAlias`, `nsFull`, attr, tpe, colType, card, _, _, _, attrExpr, _, _)
      if attrExpr != "edit" =>
      val (tpeProcess, tpeTransfer, paramConverter) = attrTokens(attr, tpe, card)
      lhsTypesProcess += tpeProcess
      lhsParamsTypesProcess += s"$attr: $tpeProcess"
      lhsTypes += tpeTransfer
      lhsParamsTypes += s"$attr: $tpeTransfer"
      lhsParams += paramConverter
      colTypes += colType
      colIndexes += colIndex

    case Col(colIndex, _, nsAlias, _, attr, tpe, colType, card, _, _, _, attrExpr, _, _)
      if attrExpr != "edit" =>
      val Ns_attr                                   = nsAlias + "_" + attr
      val (tpeProcess, tpeTransfer, paramConverter) = attrTokens(Ns_attr, tpe, card)
      lhsTypesProcess += tpeProcess
      lhsParamsTypesProcess += s"$Ns_attr: $tpeProcess"
      lhsTypes += tpeTransfer
      lhsParamsTypes += s"$Ns_attr: $tpeTransfer"
      lhsParams += paramConverter
      colTypes += colType
      colIndexes += colIndex
  }

  // Get input for scala right hand side code (before processing adds spinner!)
  val rhs: String = _html2str(document.getElementById(filterId).innerHTML)
    .trim.replaceAllLiterally("\n", "\n      ")

  // Start spinner since compilation can take some seconds
  processing() = filterId

  val qr         : QueryResult     = queryCache.now.find(_.modelElements == modelElements.now).get.queryResult
  val tableRows  : NodeList        = document.getElementById("tableBody").childNodes
  val attrLambdas: Seq[Int => Any] = AttrLambdas(qr).get
  val updateCells: UpdateCells     = UpdateCells(colIndex, attrType, card, tableRows)


  private def transformValues[ColType, Ret](arrays: List[Array[Option[ColType]]],
                                            toColType: Ret => ColType,
                                            updateCell: (Int, Option[ColType], Option[ColType]) => Unit
                                           ): Unit = {
    val scalaCode = ScalaCode(
      card,
      lhsTypesProcess.mkString(", "),
      lhsParamsTypesProcess.mkString(", "),
      lhsTypes.mkString(", "),
      lhsParamsTypes.mkString(", "),
      lhsParams.mkString(", "),
      attrType,
      rhs
    ).get

    println(scalaCode)

    val scalafiddle              = ScalafiddleApi[Ret](scalaCode)
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

    val indexBridge: Int => Int = {
      if (filterIndex.nonEmpty)
        (i: Int) => filterIndex(i)
      else if (sortIndex.nonEmpty)
        (i: Int) => sortIndex(i)
      else
        (i: Int) => i
    }

    val resolve: (Int, Int => Ret) => Unit = {
      if (card == 1) {
        (i: Int, applyFn: Int => Ret) => {
          j = indexBridge(i)
          oldVopt = origArray(j)
          newVopt = applyFn(j) match {
            case "__None__" => None
            case s          => Some(toColType(s))
          }
          if (i >= tableRowIndexOffset && i < tableRowIndexMax) {
            updateCell(tableRowIndex, oldVopt, newVopt)
            tableRowIndex += 1
          }
          editArray(j) = newVopt
        }
      } else {
        (i: Int, applyFn: Int => Ret) => {
          j = indexBridge(i)
          oldVopt = origArray(j)
          newVopt = applyFn(j) match {
            case Nil => None
            case vs  => Some(toColType(vs))
          }
          if (i >= tableRowIndexOffset && i < tableRowIndexMax) {
            updateCell(tableRowIndex, oldVopt, newVopt)
            tableRowIndex += 1
          }
          editArray(j) = newVopt
        }
      }
    }

    CalculateGroupEdit(colIndexes, attrLambdas, scalafiddle, lastRow, resolve)

    // Group edit completed - stop spinner
    processing() = ""
  }


  // Card one ------------------------------------------

  def string(): Unit = {
    val cellBaseClass  = if (attrType == "BigInt" || attrType == "BigDecimal")
      "num" else "str"
    val toColType      = attrType match {
      case "Date" => (s: String) => date2str(new Date(s.toLong))
      case _      => (s: String) => s
    }
    val colValueToNode = attrType match {
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
    transformValues[Double, String](
      qr.num,
      (s: String) => s.toDouble,
      updateCells.cardOne("num", (v: Double) => v.render),
    )
  }


  // Card many ------------------------------------------

  def listString(): Unit = {
    val colValueToItems = attrType match {
      case "String" => (vs: List[String]) => vs.sorted.map(v => li(_str2frags(v)))
      case _        => (vs: List[String]) => vs.sorted.map(li(_))
    }
    def transform[Ret](cellBaseClass: String,
                       toColType: Ret => List[String]): Unit = transformValues(
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
      case "Date"    => transform("str", (vs: js.Array[js.Date]) =>
        vs.toList.distinct.map(jsDate => date2str(new Date(jsDate.getTime.toLong))))
      case "UUID"    => transform("str", (vs: js.Array[UUID]) =>
        vs.toList.distinct.map(_.toString))
      case "URI"     => transform("str", (vs: js.Array[URI]) =>
        vs.toList.distinct.map(_.toString))
      case _         => transform("num", (vs: js.Array[String]) =>
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
    def transform[Ret](cellBaseClass: String,
                       toColType: Ret => Map[String, String]): Unit = transformValues(
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
