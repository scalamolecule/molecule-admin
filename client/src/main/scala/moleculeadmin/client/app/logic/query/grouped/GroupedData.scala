package moleculeadmin.client.app.logic.query.grouped

import moleculeadmin.client.app.logic.query.KeyEvents
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.shared.ast.query.Col
import rx.Ctx


abstract class GroupedData[T](col: Col)(implicit ctx: Ctx.Owner)
  extends KeyEvents {

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType, _,
  opt, enums, _, _, _, _, _) = col

  val qr            = cachedQueryResult
  val attrFull      = s":$nsFull/${clean(attr)}"
  val enumPrefix    = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"
  val isNum         = Seq("Int", "Long", "Float", "Double").contains(attrType)
  val mandatory     = !opt
  val valueColIndex = colIndex + 1
  val eidIndex      = getEidColIndex(columns.now, colIndex, nsAlias, nsFull)
  val eidArray      = qr.num(qr.arrayIndexes(eidIndex))
  val arrayIndex    = qr.arrayIndexes(colIndex)
  val valueArray    = (
    if (colType == "double")
      qr.num(arrayIndex)
    else
      qr.str(arrayIndex)
    ).asInstanceOf[Array[Option[T]]]

  private var countEmpty = 0
  private var rawDoubles = Seq.empty[(Double, Int)]
  private var rawStrings = Seq.empty[(String, Int)]
  private var doubles    = List.empty[(Option[Double], Int)]
  private var strings    = List.empty[(Option[String], Int)]

  var groupedData = List.empty[(Option[T], Int)]
  val none        = "__none__"

  def rowId(rowIndex: Int) = s"grouped-row-$colIndex-$rowIndex"

  def cellId(rowIndex: Int) = s"grouped-cell-$colIndex-$rowIndex"

  def sortData(ordering: Int): Unit = {
    groupedData = (if (colType == "double") {
      val vs = if (countEmpty > 0) (None, countEmpty) +: doubles else doubles
      ordering match {
        case 1 => vs.sortBy { case (v, c) => (-c, v) }
        case 2 => vs.sortBy { case (v, c) => (c, v) }
        case 3 => vs.sortBy(_._1)
        case 4 => vs.sortBy(_._1).reverse
      }
    } else {
      val vs = if (countEmpty > 0) (None, countEmpty) +: strings else strings
      ordering match {
        case 1 => vs.sortBy { case (v, c) => (-c, v) }
        case 2 => vs.sortBy { case (v, c) => (c, v) }
        case 3 => vs.sortBy(_._1)
        case 4 => vs.sortBy(_._1).reverse
      }
    }).asInstanceOf[List[(Option[T], Int)]]
  }

  def extractGroupedData(): Unit = {
    val indexBridge = cachedIndexBridge
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
        rawStrings = vs.groupBy(identity).view.mapValues(_.length).toSeq
        strings = vs.groupBy(identity).map { case (k, v) => (Some(k), v.length) }.toList

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
        rawDoubles = vs.groupBy(identity).view.mapValues(_.length).toSeq
        doubles = vs.groupBy(identity).map { case (k, v) => (Some(k), v.length) }.toList
    }
  }
}
