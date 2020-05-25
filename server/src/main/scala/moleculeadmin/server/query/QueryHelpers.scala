package moleculeadmin.server.query

import java.lang.{Long => jLong}
import java.net.URI
import java.util.{Date, UUID}
import datomic.Util
import molecule.facade.Conn
import moleculeadmin.server.Base
import moleculeadmin.server.utils.DateStrLocal
import moleculeadmin.shared.api.QueryApi


trait QueryHelpers extends QueryApi with Base with DateStrLocal {


  // Todo: this works but seems like a hack that would be nice to avoid although
  //  the impact of a few input variables is negible.
  // To avoid type combination explosions from multiple inputs of various types
  // to be transferred with autowire/boopickle, we cast all input variable values
  // as String on the client and then cast them back to their original type here
  // and pass them as Object's to Datomic.
  def cast(pair: (String, String)): Object = pair match {
    case ("String", v)     => v.asInstanceOf[Object]
    case ("Int", v)        => v.toInt.asInstanceOf[Object]
    case ("Long", v)       => v.toLong.asInstanceOf[Object]
    case ("Float", v)      => v.toDouble.asInstanceOf[Object]
    case ("Double", v)     => v.toDouble.asInstanceOf[Object]
    case ("BigInt", v)     => BigInt.apply(v).asInstanceOf[Object]
    case ("BigDecimal", v) => BigDecimal(v).asInstanceOf[Object]
    case ("Boolean", v)    => v.toBoolean.asInstanceOf[Object]
    case ("Date", v)       => str2date(v).asInstanceOf[Object]
    case ("UUID", v)       => java.util.UUID.fromString(v).asInstanceOf[Object]
    case ("URI", v)        => new java.net.URI(v).asInstanceOf[Object]
    case _                 => sys.error("Unexpected input pair to cast")
  }

  def inputs(lists: Seq[(Int, AnyRef)]): Seq[Object] = {
    lists.sortBy(_._1).map(_._2).map {
      case l: Seq[_] => Util.list(l.map {
        case l2: Seq[_] =>
          Util.list(l2.map(v => cast(v.asInstanceOf[(String, String)])): _*)

        case pair@(_: String, _: String) =>
          cast(pair.asInstanceOf[(String, String)])

        case _ => sys.error("Unexpected input values")
      }: _*)

      case pair@(_: String, _: String) =>
        cast(pair.asInstanceOf[(String, String)])

      case _ =>
        sys.error("Unexpected input values")
    }
  }


  protected def formatEmpty(v: Any): String = {
    val s = v.toString
    if (s.trim.isEmpty) s"{$s}" else s
  }

  protected def ident(conn: Conn, e: jLong): String =
    conn.db.entity(e).get(":db/ident").toString

  protected def formatValue(
    conn: Conn,
    attr: String,
    v: Any,
    enumAttrs: Seq[String]
  ): String = v match {
    case s: String                            => formatEmpty(s)
    case e: jLong if enumAttrs.contains(attr) => ident(conn, e)
    case d: Date                              => date2strLocal(d)
    case v                                    => formatEmpty(v)
  }


  def getCaster(tpe: String, enumPrefix: String): String => Any = {
    tpe match {
      case "String"               => (v: String) => enumPrefix + v
      case "Int" | "Long" | "ref" => (v: String) => v.toLong
      case "Float" | "Double"     => (v: String) => v.toDouble
      case "Boolean"              => (v: String) => v.toBoolean
      case "Date"                 => (v: String) => str2date(v)
      case "UUID"                 => (v: String) => UUID.fromString(v)
      case "URI"                  => (v: String) => new URI(v)
      case "BigInt"               => (v: String) => BigInt(v)
      case "BigDecimal"           => (v: String) => BigDecimal(v)
    }
  }

  def getStrCaster(tpe: String, enumPrefix: String): String => Any = {
    tpe match {
      case "String"     => (v: String) => enumPrefix + v
      case "Boolean"    => (v: String) => v.toBoolean
      case "Date"       => (v: String) => str2date(v)
      case "UUID"       => (v: String) => UUID.fromString(v)
      case "URI"        => (v: String) => new URI(v)
      case "BigInt"     => (v: String) => BigInt(v)
      case "BigDecimal" => (v: String) => BigDecimal(v)
    }
  }

  def getNumCaster(tpe: String): Double => Any = {
    tpe match {
      case "Int" | "Long" | "ref" => (v: Double) => v.toLong
      case "Float" | "Double"     => (v: Double) => v
    }
  }
}
