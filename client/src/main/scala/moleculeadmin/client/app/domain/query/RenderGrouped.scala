package moleculeadmin.client.app.domain.query

import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.GroupedAttrElements
import moleculeadmin.shared.ast.query.QueryResult
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


case class RenderGrouped()(implicit val ctx: Ctx.Owner)
  extends GroupedAttrElements {


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    groupedColIndexes()
    if (showGrouped && groupedColIndexes.now.nonEmpty) {
      _cardsContainer(
        columns.now.collect {
          case c
            if groupedColIndexes.now.contains(c.colIndex)
              && groupableCols.map(_.colIndex).contains(c.colIndex)
          =>
            _grouped(
              s"${c.nsFull}/${c.attr}",
              groupedTable(c.colIndex, c.attr, c.colType)
            )
        }
      )
    } else {
      span()
    }
  }

  def groupedTable(
    colIndex: Int,
    attr: String,
    colType: String,
  ): TypedTag[Element] = {
    val mandatory = !attr.endsWith("$")
    _groupedTable(
      colType,
      mandatory,
      groupData(queryCache.queryResult, colIndex, colType, mandatory)
    )
  }

  def cacheGrouped() = {}

  def groupData(
    qr: QueryResult,
    colIndex: Int,
    colType: String,
    mandatory: Boolean
  ): Seq[(String, Int)] = {
    colType match {
      case "string" if mandatory =>
        qr.str(qr.arrayIndexes(colIndex)).toList.flatten.map {
          // todo: format empty string/line shifts
          case v if v.trim.isEmpty => s"{$v}"
          case v                   => v
        }.groupBy(identity).mapValues(_.length).toSeq
          .sortBy { case (v, c) => (-c, v) }

      case "double" if mandatory =>
        qr.num(qr.arrayIndexes(colIndex)).toList.flatten
          .groupBy(identity).mapValues(_.length).toSeq
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
