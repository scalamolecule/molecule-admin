package moleculeadmin.client.app.domain.query.data.groupedit.ops
import java.net.URI
import java.time.LocalDateTime
import java.util.{Date, UUID}
import molecule.util.DateHandling
import moleculeadmin.client.app.domain.query.QueryState.columns
import moleculeadmin.shared.ast.query.QueryResult
import moleculeadmin.shared.util.HelpersAdmin
import scala.collection.immutable.Map
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.UndefOr

case class ToTransferValueLambdas(qr: QueryResult) extends HelpersAdmin {

  val arrayIndexes: Map[Int, Int] = qr.arrayIndexes

  def get: Seq[Int => Any] = {
    columns.now.collect {
      case col if col.attrExpr != "edit" =>
        getToTransferValueLambda(col.colType, col.colIndex, col.attrType, col.attr.last == '$')
    }
  }

  def getToTransferValueLambda(
    colType: String,
    colIndex: Int,
    attrType: String,
    opt: Boolean
  ): Int => Any = colType match {
    case "string" =>
      val array = qr.str(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "String"                => (j: Int) =>
            array(j).orUndefined
          case "Boolean"               => (j: Int) =>
            array(j).fold(Option.empty[Boolean])(v => Some(v.toBoolean)).orUndefined
          case "Date"                  => (j: Int) =>
            array(j).fold(Option.empty[Date])(v => Some(str2date(v))).orUndefined
          case "UUID"                  => (j: Int) =>
            array(j).fold(Option.empty[UUID])(v => Some(UUID.fromString(v))).orUndefined
          case "URI"                   => (j: Int) =>
            array(j).fold(Option.empty[URI])(v => Some(new URI(v))).orUndefined
          case "BigInt" | "BigDecimal" => (j: Int) =>
            array(j).orUndefined
        }
      else
        attrType match {
          case "String"                => (j: Int) =>
            array(j).get
          case "Boolean"               => (j: Int) =>
            array(j).get.toBoolean
          case "Date"                  => (j: Int) =>
            str2ldt(array(j).get)
          case "UUID"                  => (j: Int) =>
            UUID.fromString(array(j).get)
          case "URI"                   => (j: Int) =>
            new URI(array(j).get)
          case "BigInt" | "BigDecimal" => (j: Int) =>
            array(j).get
        }

    case "double" =>
      val array = qr.num(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "Int" => (j: Int) =>
            array(j).fold(Option.empty[Int])(v => Some(v.toInt)).orUndefined
          case _     => (j: Int) =>
            array(j).fold(Option.empty[String])(v => Some(v.toString)).orUndefined
        }
      else
        attrType match {
          case "Int" => (j: Int) => array(j).get.toInt
          case _     => (j: Int) => array(j).get.toString
        }

    case "listString" =>
      val array = qr.listStr(arrayIndexes(colIndex))
      attrType match {
        case "String" => (j: Int) =>
          array(j).fold(new js.Array[String](0))(_.toJSArray)

        case "Boolean" => (j: Int) =>
          array(j).fold(new js.Array[Boolean](0))(vs =>
            vs.map(_.toBoolean).toJSArray)

        case "Date" => (j: Int) =>
          array(j).fold(new js.Array[LocalDateTime](0))(vs =>
            vs.map(str2ldt).toJSArray)

        case "UUID" => (j: Int) =>
          array(j).fold(new js.Array[UUID](0))(vs =>
            vs.map(UUID.fromString).toJSArray)

        case "URI" => (j: Int) =>
          array(j).fold(new js.Array[URI](0))(vs =>
            vs.map(new URI(_)).toJSArray)

        case "BigInt" => (j: Int) =>
          array(j).fold(new js.Array[BigInt](0))(vs =>
            vs.map(BigInt(_)).toJSArray)

        case "BigDecimal" => (j: Int) =>
          array(j).fold(new js.Array[BigDecimal](0))(vs =>
            vs.map(BigDecimal(_)).toJSArray)
      }

    case "listDouble" =>
      val array = qr.listNum(arrayIndexes(colIndex))
      attrType match {
        case "Int" => (j: Int) =>
          array(j).fold(new js.Array[Int](0))(vs =>
            vs.map(_.toInt).toJSArray)

        // Long, Float, Double are transferred as String (for BigInt/BigDecimal)
        case "Long" => (j: Int) =>
          array(j).fold(new js.Array[String](0))(vs =>
            vs.map(_.toString).toJSArray)
      }

    case "mapString" =>
      val array = qr.mapStr(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "String"                => (j: Int) =>
            //            val x: Option[Map[String, String]] = array(j)
            array(j).toJSArray
          case "Boolean"               => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, Boolean]])(pairs =>
                Some(pairs.map { case (k, v) => k -> v.toBoolean })).toJSArray
          case "Date"                  => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, Date]])(pairs =>
                Some(pairs.map { case (k, v) => k -> str2date(v) })).toJSArray
          case "UUID"                  => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, UUID]])(pairs =>
                Some(pairs.map { case (k, v) => k -> UUID.fromString(v) })).toJSArray
          case "URI"                   => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, URI]])(pairs =>
                Some(pairs.map { case (k, v) => k -> new URI(v) })).toJSArray
          case "BigInt" | "BigDecimal" => (j: Int) => array(j).toJSArray
        }
      else
        attrType match {
          case "String"                => (j: Int) =>
            array(j).get
          case "Boolean"               => (j: Int) =>
            array(j).get.map { case (k, v) => k -> v.toBoolean }
          case "Date"                  => (j: Int) =>
            array(j).get.map {
              case (k, v) => k -> new js.Date(str2date(v).getTime.toDouble)
            }
          case "UUID"                  => (j: Int) =>
            array(j).get.map { case (k, v) => k -> UUID.fromString(v) }
          case "URI"                   => (j: Int) =>
            array(j).get.map { case (k, v) => k -> new URI(v) }
          case "BigInt" | "BigDecimal" => (j: Int) =>
            array(j).get
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
          case "Int" => (j: Int) =>
            array(j).get.map { case (k, v) => k -> v.toInt }
          case _     => (j: Int) =>
            array(j).get.map { case (k, v) => k -> v.toString }
        }
  }
}
