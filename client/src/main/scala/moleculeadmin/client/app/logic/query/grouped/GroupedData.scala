package moleculeadmin.client.app.logic.query.grouped

import moleculeadmin.client.app.logic.query.KeyEvents
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.shared.ast.query.Col
import rx.Ctx


abstract class GroupedData[T](col: Col)(implicit ctx: Ctx.Owner)
  extends KeyEvents {

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType, card,
  opt, enums, _, _, _, _, _, _) = col

  val qr         = cachedQueryResult
  val arrayIndex = qr.arrayIndexes(colIndex)
  val valueArray = (colType match {
    case "string"     => qr.str(arrayIndex)
    case "double"     => qr.num(arrayIndex)
    case "listString" => qr.listStr(arrayIndex)
    case "listDouble" => qr.listNum(arrayIndex)
    case "mapString"  => qr.mapStr(arrayIndex)
    case "mapDouble"  => qr.mapNum(arrayIndex)
  }).asInstanceOf[Array[Option[T]]]

  private var countEmpty = 0

  // For sorting by number/string
  private var strings       = List.empty[(Option[String], Int)]
  private var doubles       = List.empty[(Option[Double], Int)]
  private var mappedStrings = List.empty[(Option[(String, String)], Int)]
  private var mappedDoubles = List.empty[(Option[(String, Double)], Int)]

  var groupedData = List.empty[(Option[String], Int)]


  def sortData(ordering: Int): Unit = {
    groupedData = colType match {
      case "string" | "listString" =>
        val vs = if (countEmpty > 0) (None, countEmpty) +: strings else strings
        ordering match {
          case 1 => vs.sortBy { case (v, c) => (-c, v) }
          case 2 => vs.sortBy { case (v, c) => (c, v) }
          case 3 => vs.sortBy(_._1)
          case 4 => vs.sortBy(_._1).reverse
        }

      case "double" | "listDouble" =>
        val vs = if (countEmpty > 0) (None, countEmpty) +: doubles else doubles
        (ordering match {
          case 1 => vs.sortBy { case (v, c) => (-c, v) }
          case 2 => vs.sortBy { case (v, c) => (c, v) }
          case 3 => vs.sortBy(_._1)
          case 4 => vs.sortBy(_._1).reverse
        }).map { case (v, c) => (v.map(_.toString), c) }

      case "mapString" =>
        val vs     = if (countEmpty > 0) (None, countEmpty) +: mappedStrings else mappedStrings
        val sorted = ordering match {
          case 1 => vs.sortBy { case (v, c) => (-c, v) }
          case 2 => vs.sortBy { case (v, c) => (c, v) }
          case 3 => vs.sortBy(_._1)
          case 4 => vs.sortBy(_._1).reverse
        }
        sorted.map {
          case (None, c)         => (None, c)
          case (Some((k, v)), c) => (Some(s"$k -> $v"), c)
        }

      case "mapDouble" =>
        val vs     = if (countEmpty > 0) (None, countEmpty) +: mappedDoubles else mappedDoubles
        val sorted = ordering match {
          case 1 => vs.sortBy { case (v, c) => (-c, v) }
          case 2 => vs.sortBy { case (v, c) => (c, v) }
          case 3 => vs.sortBy(_._1)
          case 4 => vs.sortBy(_._1).reverse
        }
        sorted.map {
          case (None, c)         => (None, c)
          case (Some((k, v)), c) => (Some(s"$k -> $v"), c)
        }
    }
  }

  def extractGroupedData(): Unit = {
    val indexBridge = cachedIndexBridge
    var rowIndex    = 0
    val lastRow     = actualRowCount
    colType match {
      case "string" =>
        val valueArray1 = valueArray.asInstanceOf[Array[Option[String]]]
        var vs          = List.empty[String]
        while (rowIndex < lastRow) {
          valueArray1(indexBridge(rowIndex)) match {
            case None    => countEmpty += 1
            case Some(v) => vs = v :: vs
          }
          rowIndex += 1
        }
        strings = vs.groupBy(identity).map {
          case (v, simVs) => (Some(v), simVs.length)
        }.toList

      case "double" =>
        val valueArray1 = valueArray.asInstanceOf[Array[Option[Double]]]
        var vs          = List.empty[Double]
        while (rowIndex < lastRow) {
          valueArray1(indexBridge(rowIndex)) match {
            case None    => countEmpty += 1
            case Some(v) => vs = v :: vs
          }
          rowIndex += 1
        }
        doubles = vs.groupBy(identity).map {
          case (v, simVs) => (Some(v), simVs.length)
        }.toList


      case "listString" =>
        val valueArray1 = valueArray.asInstanceOf[Array[Option[List[String]]]]
        var vs          = List.empty[String]
        while (rowIndex < lastRow) {
          valueArray1(indexBridge(rowIndex)) match {
            case None     => countEmpty += 1
            case Some(vv) => vs = vv ::: vs
          }
          rowIndex += 1
        }
        strings = vs.groupBy(identity).map {
          case (v, simVs) => (Some(v), simVs.length)
        }.toList

      case "listDouble" =>
        val valueArray1 = valueArray.asInstanceOf[Array[Option[List[Double]]]]
        var vs          = List.empty[Double]
        while (rowIndex < lastRow) {
          valueArray1(indexBridge(rowIndex)) match {
            case None     => countEmpty += 1
            case Some(vv) => vs = vv ::: vs
          }
          rowIndex += 1
        }
        doubles = vs.groupBy(identity).map {
          case (v, simVs) => (Some(v), simVs.size)
        }.toList


      case "mapString" =>
        val valueArray1 = valueArray.asInstanceOf[Array[Option[Map[String, String]]]]
        var pairs       = List.empty[(String, String)]
        while (rowIndex < lastRow) {
          valueArray1(indexBridge(rowIndex)) match {
            case None     => countEmpty += 1
            case Some(vv) => pairs = vv.toList ::: pairs
          }
          rowIndex += 1
        }
        mappedStrings = pairs.groupBy(identity).map {
          case (pair, simPairs) => (Some(pair), simPairs.size)
        }.toList

      case "mapDouble" =>
        val valueArray1 = valueArray.asInstanceOf[Array[Option[Map[String, Double]]]]
        var pairs       = List.empty[(String, Double)]
        while (rowIndex < lastRow) {
          valueArray1(indexBridge(rowIndex)) match {
            case None     => countEmpty += 1
            case Some(vv) => pairs = vv.toList ::: pairs
          }
          rowIndex += 1
        }
        mappedDoubles = pairs.groupBy(identity).map {
          case (pair, simPairs) => (Some(pair), simPairs.size)
        }.toList
    }
  }
}
