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

  private var rawData = Seq.empty[(T, Int)]
  var groupedData = Seq.empty[(String, Int)]


  def rowId(rowIndex: Int) = s"grouped-row-$colIndex-$rowIndex"

  def cellId(rowIndex: Int) = s"grouped-cell-$colIndex-$rowIndex"

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
        val vs         = new Array[String](lastRow)
        while (rowIndex < lastRow) {
          vs(rowIndex) = valueArray(indexBridge(rowIndex)).get match {
            case v if v.trim.isEmpty => s"{$v}"
            case v                   => v
          }
          rowIndex += 1
        }
        vs.groupBy(identity).mapValues(_.length).toSeq

      case "double" if mandatory =>
        val valueArray = qr.num(qr.arrayIndexes(colIndex))
        var rowIndex   = 0
        val lastRow    = actualRowCount
        val vs         = new Array[Double](lastRow)
        while (rowIndex < lastRow) {
          vs(rowIndex) = valueArray(indexBridge(rowIndex)).get
          rowIndex += 1
        }
        vs.groupBy(identity).mapValues(_.length).toSeq


      case "string" =>
        val valueArray = qr.str(qr.arrayIndexes(colIndex))
        var empty      = 0
        var vs         = List.empty[String]
        var rowIndex   = 0
        val lastRow    = actualRowCount
        while (rowIndex < lastRow) {
          valueArray(indexBridge(rowIndex)) match {
            case None                      => empty += 1
            case Some(v) if v.trim.isEmpty => vs = s"{$v}" :: vs
            case Some(v)                   => vs = v :: vs
          }
          rowIndex += 1
        }
        ("-", empty) +: vs.groupBy(identity).mapValues(_.length).toSeq

      case "double" =>
        val valueArray = qr.num(qr.arrayIndexes(colIndex))
        var empty      = 0
        var vs         = List.empty[Double]
        var rowIndex   = 0
        val lastRow    = actualRowCount
        while (rowIndex < lastRow) {
          valueArray(indexBridge(rowIndex)) match {
            case None    => empty += 1
            case Some(v) => vs = v :: vs
          }
          rowIndex += 1
        }
        ("-", empty) +: vs.groupBy(identity).mapValues(_.length).toSeq

      case tpe =>
        throw new IllegalStateException("Unsupported type: " + tpe)
    }).asInstanceOf[Seq[(T, Int)]]
  }
}
