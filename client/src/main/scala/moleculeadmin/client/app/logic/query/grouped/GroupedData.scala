package moleculeadmin.client.app.logic.query.grouped

import moleculeadmin.client.app.logic.query.KeyEvents
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.shared.ast.query.Col
import rx.Ctx
import scalatags.JsDom.all.s


abstract class GroupedData[T](col: Col)(implicit ctx: Ctx.Owner)
  extends KeyEvents {

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType, _,
  opt, enums, _, _, _, _, _) = col

  val qr            = queryCache.queryResult
  val attrFull      = s":$nsFull/${clean(attr)}"
  val enumPrefix    = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"
  val isNum         = Seq("Int", "Long", "Float", "Double").contains(attrType)
  val mandatory     = !opt
  val valueColIndex = colIndex + 1
  val eidIndex      = getEidColIndex(columns.now, colIndex, nsAlias, nsFull)
  val eidArray      = qr.num(eidIndex)
  val arrayIndex    = qr.arrayIndexes(colIndex)
  val valueArray    = (
    if (colType == "double") qr.num(arrayIndex) else qr.str(arrayIndex)
    ).asInstanceOf[Array[Option[T]]]

  private var rawData    = Seq.empty[(T, Int)]
  private var countEmpty = 0
  private var rawDoubles = Seq.empty[(Double, Int)]
  private var rawStrings = Seq.empty[(String, Int)]

  private var doubles = List.empty[(Option[Double], Int)]
  private var strings = List.empty[(Option[String], Int)]
  var groupedData = List.empty[(Option[T], Int)]
  val none        = "__none__"


  def rowId(rowIndex: Int) = s"grouped-row-$colIndex-$rowIndex"

  def cellId(rowIndex: Int) = s"grouped-cell-$colIndex-$rowIndex"

  def sortData2(ordering: Int): Unit = {
    groupedData = (if (colType == "double") {
      val vs = if (countEmpty > 0) (None, countEmpty) +: doubles else doubles
      ordering match {
        case 1 => vs.sortBy { case (v, c) => (-c, v) }
        case 2 => vs.sortBy { case (v, c) => (c, v) }
        case 3 => vs.sortBy(_._1)
        case 4 => vs.sortBy(_._1).reverse
      }
    }else {
      val vs = if (countEmpty > 0) (None, countEmpty) +: strings else strings
      ordering match {
        case 1 => vs.sortBy { case (v, c) => (-c, v) }
        case 2 => vs.sortBy { case (v, c) => (c, v) }
        case 3 => vs.sortBy(_._1)
        case 4 => vs.sortBy(_._1).reverse
      }
    }).asInstanceOf[List[(Option[T], Int)]]
  }

  //  def sortData3(ordering: Int): Unit = {
  //    groupedData = if (colType == "double") {
  //      (ordering match {
  //        case 1 => rawDoubles.sortBy { case (v, c) => (-c, v) }
  //        case 2 => rawDoubles.sortBy { case (v, c) => (c, v) }
  //        case 3 => rawDoubles.sortBy(_._1)
  //        case 4 => rawDoubles.sortBy(-_._1)
  //      }).map { case (v, c) => (v.toString, c) }
  //    } else {
  //      if (countEmpty > 0)
  //        rawStrings = (none, countEmpty) +: rawStrings
  //      ordering match {
  //        case 1 => rawStrings.sortBy { case (v, c) => (-c, v) }
  //        case 2 => rawStrings.sortBy { case (v, c) => (c, v) }
  //        case 3 => rawStrings.sortBy(_._1)
  //        case 4 => rawStrings.sortBy(_._1).reverse
  //      }
  //      //      if (countEmpty == 0) {
  //      //      } else {
  //      //        ordering match {
  //      //          case 1 =>
  //      //            rawStrings =
  //      //            rawStrings.sortBy { case (v, c) => (-c, v) }
  //      //          case 2 => rawStrings.sortBy { case (v, c) => (c, v) }
  //      //          case 3 => rawStrings.sortBy(_._1)
  //      //          case 4 => rawStrings.sortBy(_._1).reverse
  //      //        }
  //      //      }
  //    }
  //
  //
  //    println("----- groupedData2")
  //    groupedData foreach println
  //  }

  //  def sortData(ordering: Int): Unit = {
  //    groupedData =
  //      if (colType == "double") {
  //        val data = rawData.asInstanceOf[Seq[(Double, Int)]]
  //        (ordering match {
  //          case 1 => data.sortBy { case (v, c) => (-c, v) }
  //          case 2 => data.sortBy { case (v, c) => (c, v) }
  //          case 3 => data.sortBy(_._1)
  //          case 4 => data.sortBy(_._1).reverse
  //        }).map { case (v, c) => (v.toString, c) }
  //      } else {
  //        val data = rawData.asInstanceOf[Seq[(String, Int)]]
  //        ordering match {
  //          case 1 => data.sortBy { case (v, c) => (-c, v) }
  //          case 2 => data.sortBy { case (v, c) => (c, v) }
  //          case 3 => data.sortBy(_._1)
  //          case 4 => data.sortBy(_._1).reverse
  //        }
  //      }
  //  }

  def extractGroupedData(): Unit = {
    val filterIndex = queryCache.filterIndex
    val indexBridge = {
      if (filterIndex.nonEmpty)
        (i: Int) => filterIndex(i)
      else
        (i: Int) => i
    }
    var rowIndex    = 0
    val lastRow     = actualRowCount
    colType match {
      case "string" =>
        val valueArray = qr.str(qr.arrayIndexes(colIndex))
        var vs         = List.empty[String]
        groupedData = Nil
        while (rowIndex < lastRow) {
          valueArray(indexBridge(rowIndex)) match {
            case None    => countEmpty += 1
            case Some(v) => vs = (if (v.trim.isEmpty) s"{$v}" else v) :: vs
          }
          rowIndex += 1
        }
        rawStrings = vs.groupBy(identity).mapValues(_.length).toSeq
        strings = vs.groupBy(identity).map { case (k, v) => (Some(k), v.length) }.toList

//        println("---------- rawStrings")
//        rawStrings foreach println

      case "double" =>
        val valueArray = qr.num(qr.arrayIndexes(colIndex))
        var vs         = List.empty[Double]
        while (rowIndex < lastRow) {
          valueArray(indexBridge(rowIndex)) match {
            case None    => countEmpty += 1
            case Some(v) => vs = v :: vs
          }
          rowIndex += 1
        }
        rawDoubles = vs.groupBy(identity).mapValues(_.length).toSeq
        doubles = vs.groupBy(identity).map { case (k, v) => (Some(k), v.length) }.toList
    }


    //        rawData = (colType match {
    //          case "string" if mandatory =>
    //            val valueArray = qr.str(qr.arrayIndexes(colIndex))
    //            var rowIndex   = 0
    //            val lastRow    = actualRowCount
    //            val vs         = new Array[String](lastRow)
    //            while (rowIndex < lastRow) {
    //              //          vs(rowIndex) = valueArray(indexBridge(rowIndex)).get match {
    //              //            case v if v.trim.isEmpty => s"{$v}"
    //              //            case v                   => v
    //              //          }
    //              vs(rowIndex) = valueArray(indexBridge(rowIndex)) match {
    //                case None                      => ""
    //                case Some(v) if v.trim.isEmpty => s"{$v}"
    //                case Some(v)                   => v
    //              }
    //              rowIndex += 1
    //            }
    //            vs.groupBy(identity).mapValues(_.length).toSeq
    //
    //          case "double" if mandatory =>
    //            val valueArray = qr.num(qr.arrayIndexes(colIndex))
    //            var rowIndex   = 0
    //            val lastRow    = actualRowCount
    //            val vs         = new Array[Double](lastRow)
    //            while (rowIndex < lastRow) {
    //              vs(rowIndex) = valueArray(indexBridge(rowIndex)).get
    //              rowIndex += 1
    //            }
    //            vs.groupBy(identity).mapValues(_.length).toSeq
    //
    //
    //          case "string" =>
    //            val valueArray = qr.str(qr.arrayIndexes(colIndex))
    //            var empty      = 0
    //            var vs         = List.empty[String]
    //            var rowIndex   = 0
    //            val lastRow    = actualRowCount
    //            while (rowIndex < lastRow) {
    //              valueArray(indexBridge(rowIndex)) match {
    //                case None                      => empty += 1
    //                case Some(v) if v.trim.isEmpty => vs = s"{$v}" :: vs
    //                case Some(v)                   => vs = v :: vs
    //              }
    //              rowIndex += 1
    //            }
    //            (none, empty) +: vs.groupBy(identity).mapValues(_.length).toSeq
    //
    //          case "double" =>
    //            val valueArray = qr.num(qr.arrayIndexes(colIndex))
    //            var empty      = 0
    //            var vs         = List.empty[Double]
    //            var rowIndex   = 0
    //            val lastRow    = actualRowCount
    //            while (rowIndex < lastRow) {
    //              valueArray(indexBridge(rowIndex)) match {
    //                case None    => empty += 1
    //                case Some(v) => vs = v :: vs
    //              }
    //              rowIndex += 1
    //            }
    //            (none, empty) +: vs.groupBy(identity).mapValues(_.length).toSeq
    //
    //          case tpe =>
    //            throw new IllegalStateException("Unsupported type: " + tpe)
    //        }).asInstanceOf[Seq[(T, Int)]]
  }
}
