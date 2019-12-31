package moleculeadmin.client.app.domain.query.grouped

import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.shared.ast.query.Col
import rx.Ctx


abstract class GroupedData[T](col: Col)(implicit ctx: Ctx.Owner)
  extends KeyEvents {

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType, _,
  opt, enums, _, _, _, _, _) = col

  val qr = queryCache.queryResult
  val attrFull   = s":$nsFull/${clean(attr)}"
  val enumPrefix = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"
  val isNum      = Seq("Int", "Long", "Float", "Double").contains(attrType)
  val mandatory  = !opt

  private var rawData = Seq.empty[(T, Int)]
  var groupedData = Seq.empty[(String, Int)]


  def sortData(ordering: Int): Unit = {
    groupedData =
      if (colType == "double") {
        val data = rawData.asInstanceOf[Seq[(Double, Int)]]
        (ordering match {
          case 1 => data.sortBy { case (v, c) => (-c, v) }
          case 2 => data.sortBy { case (v, c) => (c, v) }
          case 3 => data.sortBy(_._1)
          case 4 => data.sortBy(_._1).reverse
        }).map { case (v, c) => (v.toString, c) }
      } else {
        val data = rawData.asInstanceOf[Seq[(String, Int)]]
        ordering match {
          case 1 => data.sortBy { case (v, c) => (-c, v) }
          case 2 => data.sortBy { case (v, c) => (c, v) }
          case 3 => data.sortBy(_._1)
          case 4 => data.sortBy(_._1).reverse
        }
      }
  }

  def extractGroupedData(): Unit = {
    val filterIndex = queryCache.filterIndex
    val indexBridge = {
      if (filterIndex.nonEmpty)
        (i: Int) => filterIndex(i)
      else
        (i: Int) => i
    }
    rawData = (colType match {
      case "string" if mandatory =>
        val valueArray = qr.str(qr.arrayIndexes(colIndex))
        var rowIndex   = 0
        val lastRow    = actualRowCount
        val vs1        = new Array[String](lastRow)
        while (rowIndex < lastRow) {
          vs1(rowIndex) = valueArray(indexBridge(rowIndex)).get match {
            case v if v.trim.isEmpty => s"{$v}"
            case v                   => v
          }
          rowIndex += 1
        }
        vs1.groupBy(identity).mapValues(_.length).toSeq

      case "double" if mandatory =>
        val valueArray = qr.num(qr.arrayIndexes(colIndex))
        var rowIndex   = 0
        val lastRow    = actualRowCount
        val vs1        = new Array[Double](lastRow)
        while (rowIndex < lastRow) {
          vs1(rowIndex) = valueArray(indexBridge(rowIndex)).get
          rowIndex += 1
        }
        vs1.groupBy(identity).mapValues(_.length).toSeq


      case "string" =>
        val (nil, vs) = qr.str(qr.arrayIndexes(colIndex)).toList.foldLeft(
          List.empty[Int], List.empty[String]
        ) {
          case ((nil, vs), None) => (nil :+ 1, vs)
          // todo: format empty string/line shifts
          case ((nil, vs), Some(v)) if v.trim.isEmpty => (nil, vs :+ s"{$v}")
          case ((nil, vs), Some(v))                   => (nil, vs :+ v)
        }
        ("<nil>", nil.length) +: vs.groupBy(identity).mapValues(_.length).toSeq

      case "double" =>
        val (nil, vs) = qr.num(qr.arrayIndexes(colIndex)).toList.foldLeft(
          List.empty[Int], List.empty[Double]
        ) {
          case ((nil, vs), None)    => (nil :+ 1, vs)
          case ((nil, vs), Some(v)) => (nil, vs :+ v)
        }
        ("<nil>", nil.length) +: vs.groupBy(identity).mapValues(_.length).toSeq

      case _ => Nil
    }).asInstanceOf[Seq[(T, Int)]]
  }
}
