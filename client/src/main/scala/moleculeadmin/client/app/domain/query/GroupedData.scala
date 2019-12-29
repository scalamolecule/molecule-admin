package moleculeadmin.client.app.domain.query

import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.GroupedAttrElements
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.data.FilterFactory
import org.scalajs.dom.html.{Span, TableSection}
import org.scalajs.dom.{document, window}
import rx.Ctx


case class GroupedData(
  qr: QueryResult,
  col: Col,
  grouped: Span,
)(implicit ctx: Ctx.Owner)
  extends GroupedAttrElements with KeyEvents with FilterFactory {

  val (colIndex, colType, attrType, mandatory) =
    (col.colIndex, col.colType, col.attrType, !col.opt)

  // Local cache of selected pairs of rowNumber/grouped value
  var selected = Seq.empty[(Int, String)]

  def showGrouped(): Unit = {
    grouped.appendChild(_groupedSelected(selected, toggleOff))
    grouped.appendChild(_separator)
  }

  def setFilter(): Unit = {
    val filter = createFilter(
      col,
      "/" + selected.map(_._2).mkString("\n/"),
      Set.empty[Long], Set.empty[Long], Set.empty[Long],
      false
    ).get
    filters() = filters.now.filterNot(_._1 == colIndex) + (colIndex -> filter)
  }

  def toggleOn(n: Int, curV: String): Unit = {
    document.getElementById(s"grouped-$n").setAttribute("class", "selected")
    grouped.innerHTML = ""
    selected = selected :+ (n, curV)
    showGrouped()
    setFilter()
  }

  def toggleOff(n: Int, curV: String): Unit = {
    document.getElementById(s"grouped-$n").removeAttribute("class")
    grouped.innerHTML = ""
    selected = selected.filterNot(_ == (n, curV))
    if (selected.isEmpty) {
      filters() = filters.now - colIndex
    } else {
      showGrouped()
      setFilter()
    }
  }

  def appendRows(
    tableBody: TableSection,
    groupedData: Seq[(String, Int)],
    first: Int,
    last: Int
  ): Unit = {
    val update = (curV: String) => () => window.alert("update cur value: " + curV)

    val toggle = (n: Int, curV: String) => () =>
      if (selected.contains((n, curV)))
        toggleOff(n, curV)
      else
        toggleOn(n, curV)

    val rowMaker = _rowMaker(colType, attrType, update, toggle)
    var rowIndex = first
    while (rowIndex < last) {
      tableBody.appendChild(
        rowMaker(rowIndex + 1, groupedData(rowIndex))
      )
      rowIndex += 1
    }
  }

  def getData: Seq[(String, Int)] = {
    val filterIndex             = queryCache.filterIndex
    val indexBridge: Int => Int = {
      if (filterIndex.nonEmpty)
        (i: Int) => filterIndex(i)
      else
        (i: Int) => i
    }

    colType match {
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
          .sortBy { case (v, c) => (-c, v) }

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
          .sortBy { case (v, c) => (-c, v) }
          .map { case (v, c) => (v.toString, c) }


      case "string" =>
        val (nil, vs) = qr.str(qr.arrayIndexes(colIndex)).toList.foldLeft(
          List.empty[Int], List.empty[String]
        ) {
          case ((nil, vs), None) => (nil :+ 1, vs)
          // todo: format empty string/line shifts
          case ((nil, vs), Some(v)) if v.trim.isEmpty => (nil, vs :+ s"{$v}")
          case ((nil, vs), Some(v))                   => (nil, vs :+ v)
        }
        ("<nil>", nil.length) +: vs.groupBy(identity)
          .mapValues(_.length).toSeq
          .sortBy { case (v, c) => (-c, v) }

      case "double" =>
        val (nil, vs) = qr.num(qr.arrayIndexes(colIndex)).toList.foldLeft(
          List.empty[Int], List.empty[Double]
        ) {
          case ((nil, vs), None)    => (nil :+ 1, vs)
          case ((nil, vs), Some(v)) => (nil, vs :+ v)
        }
        ("<nil>", nil.length) +: vs.groupBy(identity)
          .mapValues(_.length).toSeq
          .sortBy { case (v, c) => (-c, v) }
          .map { case (v, c) => (v.toString, c) }

      case _ => Nil
    }
  }
}
