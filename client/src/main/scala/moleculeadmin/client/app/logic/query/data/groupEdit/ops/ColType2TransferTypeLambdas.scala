package moleculeadmin.client.app.logic.query.data.groupEdit.ops

import moleculeadmin.client.app.logic.query.QueryState.columns
import moleculeadmin.shared.ast.query.QueryResult
import moleculeadmin.shared.util.HelpersAdmin
import scala.collection.immutable.Map
import scala.scalajs.js.JSConverters._

case class ColType2TransferTypeLambdas(qr: QueryResult) extends HelpersAdmin {

  val arrayIndexes: Map[Int, Int] = qr.arrayIndexes

  def get: Seq[Int => Any] = {
    columns.now.collect {
      case col if col.kind != "edit" =>
        getLambda(col.colType, col.colIndex, col.attr.last == '$')
    }
  }

  private def getLambda(
    colType: String,
    colIndex: Int,
    opt: Boolean
  ): Int => Any = colType match {
    case "string" =>
      val array = qr.str(arrayIndexes(colIndex))
      if (opt)
        (j: Int) => array(j).orUndefined
      else
        (j: Int) => array(j).get


    case "double" =>
      val array = qr.num(arrayIndexes(colIndex))
      if (opt)
        (j: Int) =>
          array(j).fold(Option.empty[String])(v => Some(v.toString)).orUndefined
      else
        (j: Int) => array(j).get.toString

    // None treated as empty array
    case "listString" =>
      val array = qr.listStr(arrayIndexes(colIndex))
      (j: Int) => array(j).getOrElse(List.empty[String]).toJSArray

    case "listDouble" =>
      val array = qr.listNum(arrayIndexes(colIndex))
      (j: Int) =>
        array(j).fold(List.empty[String])(vs => vs.map(_.toString)).toJSArray

    case "mapString" =>
      val array = qr.mapStr(arrayIndexes(colIndex))
      (j: Int) => array(j).getOrElse(Map.empty[String, String]).toJSDictionary

    case "mapDouble" =>
      val array = qr.mapNum(arrayIndexes(colIndex))
      (j: Int) =>
        array(j).fold(Map.empty[String, String]) { pairs =>
          pairs.map { case (k, v) => k -> v.toString }
        }.toJSDictionary
  }
}
