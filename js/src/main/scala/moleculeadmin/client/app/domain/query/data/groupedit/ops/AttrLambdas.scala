package moleculeadmin.client.app.domain.query.data.groupedit.ops
import java.net.URI
import java.util.{Date, UUID}
import molecule.util.DateHandling
import moleculeadmin.client.app.domain.query.QueryState.columns
import moleculeadmin.shared.ast.query.QueryResult
import scala.collection.immutable.Map
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

case class AttrLambdas(qr: QueryResult) extends DateHandling {

  val arrayIndexes: Map[Int, Int] = qr.arrayIndexes

  def get: Seq[Int => Any] = {
    columns.now.collect {
      case col if col.attrExpr != "edit" =>
        getAttrLambda(col.colType, col.colIndex, col.attrType, col.attr.last == '$')
    }
  }

  def getAttrLambda(colType: String,
                    colIndex: Int,
                    attrType: String,
                    opt: Boolean): Int => Any = colType match {
    case "string" =>
      val array = qr.str(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "String"     => (j: Int) => array(j)
          case "Boolean"    => (j: Int) => array(j).fold(Option.empty[Boolean])(v => Some(v.toBoolean))
          case "Date"       => (j: Int) => array(j).fold(Option.empty[Date])(v => Some(str2date(v)))
          case "UUID"       => (j: Int) => array(j).fold(Option.empty[UUID])(v => Some(UUID.fromString(v)))
          case "URI"        => (j: Int) => array(j).fold(Option.empty[URI])(v => Some(new URI(v)))
          case "BigInt"     => (j: Int) => array(j)
          case "BigDecimal" => (j: Int) => array(j)
        }
      else
        attrType match {
          case "String"     => (j: Int) => array(j).get
          case "Boolean"    => (j: Int) => array(j).get.toBoolean
          case "Date"       => (j: Int) => str2date(array(j).get)
          case "UUID"       => (j: Int) => UUID.fromString(array(j).get)
          case "URI"        => (j: Int) => new URI(array(j).get)
          case "BigInt"     => (j: Int) => array(j).get
          case "BigDecimal" => (j: Int) => array(j).get
        }

    case "double" =>
      val array = qr.num(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "Int" => (j: Int) => array(j).fold(Option.empty[Int])(v => Some(v.toInt))
          case _     => (j: Int) => array(j).fold(Option.empty[String])(v => Some(v.toString))
        }
      else
        attrType match {
          case "Int" => (j: Int) => array(j).get.toInt
          case _     => (j: Int) => array(j).get.toString
        }

    case "listString" =>
      val array = qr.listStr(arrayIndexes(colIndex))


      if (opt)
        attrType match {
          case "String"  => (j: Int) => array(j)
          case "Boolean" => (j: Int) => array(j).fold(Option.empty[List[Boolean]])(vs => Some(vs.map(_.toBoolean)))
          case "Date"    => (j: Int) => array(j).fold(Option.empty[List[Date]])(vs => Some(vs.map(v => str2date(v))))
          case "UUID"    => (j: Int) => array(j).fold(Option.empty[List[UUID]])(vs => Some(vs.map(v => UUID.fromString(v))))
          case "URI"     => (j: Int) => array(j).fold(Option.empty[List[URI]])(vs => Some(vs.map(v => new URI(v))))
          case _         => (j: Int) => array(j)
        }
      else
        attrType match {
          case "String"  => (j: Int) => array(j).get.toJSArray
          case "Boolean" => (j: Int) => array(j).get.map(_.toBoolean).toJSArray
          case "Date"    =>
            println("aaa")
            (j: Int) => array(j).get.map(v => new js.Date(str2date(v).getTime.toDouble)).toJSArray
          case "UUID"    => (j: Int) => array(j).get.map(v => UUID.fromString(v)).toJSArray
          case "URI"     => (j: Int) => array(j).get.map(v => new URI(v)).toJSArray
          case _         => (j: Int) => array(j).get.toJSArray
        }

    case "listDouble" =>
      val array = qr.listNum(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "Int" => (j: Int) => array(j).fold(Option.empty[List[Int]])(vs => Some(vs.map(_.toInt)))
          case _     => (j: Int) => array(j).fold(Option.empty[List[String]])(vs => Some(vs.map(_.toString)))
        }
      else
        attrType match {
          case "Int" => (j: Int) => array(j).get.map(_.toInt).toJSArray
          case _     => (j: Int) => array(j).get.map(_.toString).toJSArray
        }

    case "mapString" =>
      val array = qr.mapStr(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "String"  => (j: Int) => array(j).toJSArray
          case "Boolean" => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, Boolean]])(pairs =>
                Some(pairs.map { case (k, v) => k -> v.toBoolean })).toJSArray
          case "Date"    => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, Date]])(pairs =>
                Some(pairs.map { case (k, v) => k -> str2date(v) })).toJSArray
          case "UUID"    => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, UUID]])(pairs =>
                Some(pairs.map { case (k, v) => k -> UUID.fromString(v) })).toJSArray
          case "URI"     => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, URI]])(pairs =>
                Some(pairs.map { case (k, v) => k -> new URI(v) })).toJSArray
          case _         => (j: Int) => array(j).toJSArray
        }
      else
        attrType match {
          case "String"  => (j: Int) => array(j).get
          case "Boolean" => (j: Int) => array(j).get.map { case (k, v) => k -> v.toBoolean }
          case "Date"    => (j: Int) => array(j).get.map { case (k, v) => k -> new js.Date(str2date(v).getTime.toDouble) }
          case "UUID"    => (j: Int) => array(j).get.map { case (k, v) => k -> UUID.fromString(v) }
          case "URI"     => (j: Int) => array(j).get.map { case (k, v) => k -> new URI(v) }
          case _         => (j: Int) => array(j).get
        }

    case "mapDouble" =>
      val array = qr.mapNum(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "Int" => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, Int]])(pairs =>
                Some(pairs.map { case (k, v) => k -> v.toInt }))
          case _     => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, String]])(pairs =>
                Some(pairs.map { case (k, v) => k -> v.toString }))
        }
      else
        attrType match {
          case "Int" => (j: Int) => array(j).get.map { case (k, v) => k -> v.toInt }
          case _     => (j: Int) => array(j).get.map { case (k, v) => k -> v.toString }
        }
  }
}
