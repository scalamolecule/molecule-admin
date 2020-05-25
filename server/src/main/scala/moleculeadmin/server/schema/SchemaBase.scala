package moleculeadmin.server.schema

import db.admin.dsl.moleculeAdmin._
import molecule.api.out15._
import molecule.facade.Conn
import moleculeadmin.shared.ast.schema._

trait SchemaBase {

  def getFullNs(part: String, ns: String) = {
    if (part.isEmpty)
      throw new RuntimeException(s"Partition name can't be empty string.")
    if (ns.head.isLower)
      throw new RuntimeException(s"Expected capitalized namespace name. Found '$ns'.")
    if (part == "db.part/user")
      ns
    else
      part + "_" + ns
  }

  def getFullAttr(part: String, ns: String, attr: String): String = {
    if (attr.head.isUpper)
      throw new RuntimeException(s"Expected lowercase attribute name. Found '$attr'.")
    ":" + getFullNs(part, ns) + "/" + attr
  }

  def getEntityCount(dbConn: Conn, fullAttr: String): Int = {
    val entityCount0 = dbConn.q(s"[:find (count ?e) :where [?e $fullAttr _]]")
    if (entityCount0.isEmpty) 0 else entityCount0.head.head.asInstanceOf[Int]
  }

  def getDistinctValueCount(dbConn: Conn, fullAttr: String): Int = {
    val distinctValueCount0 = dbConn.q(s"[:find (count-distinct ?v) :where [_ $fullAttr ?v]]")
    if (distinctValueCount0.isEmpty) 0 else distinctValueCount0.head.head.asInstanceOf[Int]
  }

  def getAttrE(conn: Conn, db: String, part: String, ns: String, attr: String): Long =
    meta_Db.name_(db).Partitions.name_(part).Namespaces.name_(ns).Attrs.e.name_(attr).get(conn) match {
      case Seq(attrE) => attrE
      case Nil        => throw new RuntimeException(
        s"Couldn't find attribute `$attr` in namespace `$ns` in partition `$part` in database `$db`.")
      case multiple   => throw new RuntimeException(
        s"Unexpectedly found ${multiple.size} attributes named `$attr` in namespace `$ns` in partition `$part` in database `$db`.")
    }
}