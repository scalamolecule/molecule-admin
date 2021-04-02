package moleculeadmin.server.schema

import db.admin.dsl.moleculeAdmin._
import molecule.api.out15._
import molecule.facade.Conn
import moleculeadmin.server.Base
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Values extends SchemaBase with Base {

  def updateSchemaCounts(db: String): Unit = {

    // Process counts in the background in a separate thread
    Future {
      implicit val moleculeAdminConn = Conn(base + "/MoleculeAdmin")
      val dbConn = Conn(base + "/" + db)
      val max    = 25

      // Print results to console
      println("---- Attribute --------------------------- entity count --- distinct value count --- value samples ----------------------")

      type Meta = (Long, Int, String, Long, Int, String, Long, Int, String, String, Option[Set[String]], Option[String], Option[String])
      val schema: Seq[Meta] = meta_Db.name_(db).Partitions.e.pos.name.Namespaces.e.pos.name.Attrs.e.pos.name.tpe.enums$.refNs$.descrAttr$
        .get.sortBy(r => (r._2, r._5, r._8))

      schema.foreach { case (_, _, part, _, _, ns, attrE, _, attr, tpe, enums, refNs, descrAttr) =>
        updateAttrCountsAndValues(dbConn, part, ns, attrE, attr, tpe, enums.getOrElse(Nil).toSeq, refNs, descrAttr, max)
      }

      println("------- Namespace attribute entity counts --------------")

      schema.groupBy(_._4).values.toSeq.sortBy(r => (r.head._2, r.head._5)).foreach { nsAttrs =>
        val (part, nsE, ns0) = (nsAttrs.head._3, nsAttrs.head._4, nsAttrs.head._6.capitalize)
        val ns               = if (part == "db.part/user") s":$ns0" else s":${part}_$ns0"
        val attrs            = nsAttrs.map {
          case (_, _, part1, _, _, ns1, _, _, attr, _, _, _, _) => getFullAttr(part1, ns1, attr)
        }
        val query            = if (attrs.length == 1) {
          s"[:find (count ?e) :where [?e ${attrs.head} _]]"
        } else {
          val ors = attrs.mkString(s"[?e ", s" _]\n[?e ", " _]")
          s"[:find (count ?e) :where (or $ors)]"
        }
        val rawResult        = dbConn.q(query)
        val nsSum            = if (rawResult.isEmpty) 0 else dbConn.q(query).head.head.asInstanceOf[Int]
        if (nsSum == 0)
          println(ns + " " * (44 - ns.length) + 0)
        else
          println(ns + " " * (45 - ns.length - thousands(nsSum).length) + thousands(nsSum))
        meta_Namespace(nsE).entityCount(nsSum).update
      }

      println("------- Partition attribute entity counts --------------")

      schema.groupBy(_._1).values.toSeq.sortBy(_.head._2).foreach { partAttrs =>
        val (partE, part) = (partAttrs.head._1, partAttrs.head._3)
        val attrs         = partAttrs.map {
          case (_, _, part1, _, _, ns, _, _, attr, _, _, _, _) => getFullAttr(part1, ns, attr)
        }
        val query         = if (attrs.length == 1) {
          s"[:find (count ?e) :where [?e ${attrs.head} _]]"
        } else {
          val ors = attrs.mkString(s"[?e ", s" _]\n[?e ", " _]")
          s"[:find (count ?e) :where (or $ors)]"
        }
        val rawResult     = dbConn.q(query)
        val partSum       = if (rawResult.isEmpty) 0 else dbConn.q(query).head.head.asInstanceOf[Int]
        val part1         = if (part.isEmpty) "<1 partition>" else part
        if (partSum == 0)
          println(part1 + " " * (34 - part1.length) + 0)
        else
          println(part1 + " " * (35 - part1.length - thousands(partSum).length) + thousands(partSum))
        meta_Partition(partE).entityCount(partSum).update
      }

      println("-------------------------------------------------------")
      val allValues = meta_Db.name_(db).Partitions.entityCount(sum).get.head
      println("Total count: " + thousands(allValues))
      println("-------------------------------------------------------")
      println("done")
    }
  }

  def updateAttrCountsAndValues(dbConn: Conn, part: String, ns: String, attrE: Long, attr: String, tpe: String, enums: Seq[String], refNs: Option[String],
                                descrAttr: Option[String], max: Int = 25)(implicit moleculeAdminConn: Conn): Unit = {

    val fullAttr    = getFullAttr(part, ns, attr)
    val entityCount = getEntityCount(dbConn, fullAttr)

    if (tpe == "Boolean" && entityCount > 0) {
      print(fullAttr + " " * (55 - fullAttr.length - thousands(entityCount).length) + thousands(entityCount))

      val distinctValueCount = getDistinctValueCount(dbConn, fullAttr)
      print(" " * (20 - thousands(distinctValueCount).length) + thousands(distinctValueCount))

      val assertedValues    = getAssertedValues(dbConn, fullAttr, max)
      val assertedIds       = assertedValues.map(_._2)
      val nonAssertedValues = Seq((0, "true"), (0, "false")).filterNot(b => assertedIds.contains(b._2))
      val values            = assertedValues ++ nonAssertedValues
      val topValueIds       = stats_TopValue.entityCount.value.insert(values).eids
      meta_Attribute(attrE).entityCount(entityCount).distinctValueCount(distinctValueCount).topValues(topValueIds).update
      println("          " + values.map(_._2).mkString("     "))

    } else if (tpe == "Boolean") {
      println(fullAttr + " " * (54 - fullAttr.length) + "0                   0          true     false")

      val values      = Seq((0, "true"), (0, "false"))
      val topValueIds = stats_TopValue.entityCount.value.insert(values).eids
      meta_Attribute(attrE).entityCount().distinctValueCount().topValues(topValueIds).update

    } else if (enums.nonEmpty && entityCount > 0) {
      print(fullAttr + " " * (55 - fullAttr.length - thousands(entityCount).length) + thousands(entityCount))

      val distinctValueCount = getDistinctValueCount(dbConn, fullAttr)
      print(" " * (20 - thousands(distinctValueCount).length) + thousands(distinctValueCount))

      val assertedEnums    = getAssertedEnums(dbConn, fullAttr, max)
      val assertedIds      = assertedEnums.map(_._2)
      val allEnums         = getEnums(dbConn, fullAttr, max)
      val nonAssertedEnums = allEnums.filterNot(r => assertedIds.contains(r._2))
      val enums            = assertedEnums ++ nonAssertedEnums
      val topValueIds      = stats_TopValue.entityCount.value.label.insert(enums).eids
      meta_Attribute(attrE).entityCount(entityCount).distinctValueCount(distinctValueCount).topValues(topValueIds).update
      println("          " + enums.take(3).map(_._3).mkString("     "))

    } else if (enums.nonEmpty) {
      print(fullAttr + " " * (54 - fullAttr.length) + "0                   0")

      val allEnums    = getEnums(dbConn, fullAttr, max)
      val topValueIds = stats_TopValue.entityCount.value.label.insert(allEnums).eids
      meta_Attribute(attrE).entityCount().distinctValueCount().topValues(topValueIds).update
      println("          " + allEnums.take(3).map(_._2).mkString("     "))

    } else if (entityCount == 0) {
      println(fullAttr + " " * (54 - fullAttr.length) + 0)

      meta_Attribute(attrE).entityCount().distinctValueCount().topValues().update

    } else {
      print(fullAttr + " " * (55 - fullAttr.length - thousands(entityCount).length) + thousands(entityCount))

      val distinctValueCount = getDistinctValueCount(dbConn, fullAttr)
      print(" " * (20 - thousands(distinctValueCount).length) + thousands(distinctValueCount))

      // Retract previous top values (and sub component TopValue's)
      meta_Attribute(attrE).entityCount(entityCount).distinctValueCount(distinctValueCount).topValues().update

      if (distinctValueCount <= 25 || distinctValueCount <= 500 && entityCount != distinctValueCount || entityCount / distinctValueCount >= 2) {

        if (refNs.nonEmpty && descrAttr.nonEmpty) {
          val assertedValues = getAssertedRefLabels(dbConn, fullAttr, refNs.get, descrAttr.get, max)
          val topValueIds    = stats_TopValue.entityCount.value.label.insert(assertedValues).eids
          meta_Attribute(attrE).topValues(topValueIds).update
          println("          " + assertedValues.take(3).map(_._3).mkString("     "))

        } else {
          val assertedValues = getAssertedValues(dbConn, fullAttr, max)
          val topValueIds    = stats_TopValue.entityCount.value.insert(assertedValues).eids
          meta_Attribute(attrE).topValues(topValueIds).update
          println("          " + assertedValues.take(3).map(_._2).mkString("     "))
        }
      } else println("")
    }
  }

  def getAssertedValues(dbConn: Conn, fullAttr: String, max: Int = 25): Seq[(Int, String)] = {
    dbConn.q(s"[:find (count ?e) ?v :where [?e $fullAttr ?v]]")
      .flatMap(row => if (row(1).toString.isEmpty) None else Some((row.head.asInstanceOf[Int], row(1).toString)))
      .sortBy(_._1).reverse
      .take(max)
  }

  def getAssertedRefLabels(dbConn: Conn, fullAttr: String, refNs: String, descrAttr0: String, max: Int = 25): Seq[(Int, String, String)] = {
    val descrAttr = ":" + refNs + "/" + descrAttr0
    dbConn.q(s"[:find (count ?e) ?ref ?label :where [?e $fullAttr ?ref] [?ref $descrAttr ?label]]")
      .flatMap(row => if (row(1).toString.isEmpty) None else Some((row.head.asInstanceOf[Int], row(1).toString, row(2).toString)))
      .sortBy(_._1).reverse
      .take(max)
  }

  def getEnums(dbConn: Conn, fullAttr: String, max: Int = 25): Seq[(Int, String, String)] = {
    val enumAttrNs = fullAttr.tail.replace('/', '.')
    dbConn.q(
      s"""[:find ?ref ?label
         | :where
         |  [?ref :db/ident ?ident]
         |  [(name ?ident) ?label]
         |  [(namespace ?ident) ?ns]
         |  [(= ?ns "$enumAttrNs")]
         |]""".stripMargin)
      .map(r => (0, r.head.toString, r(1).toString))
  }

  def getAssertedEnums(dbConn: Conn, fullAttr: String, max: Int = 25): Seq[(Int, String, String)] = {
    dbConn.q(s"[:find (count ?e) ?ref ?label :where [?e $fullAttr ?ref] [?ref :db/ident ?ident] [(name ?ident) ?label]]")
      .flatMap(row => if (row(1).toString.isEmpty) None else Some((row.head.asInstanceOf[Int], row(1).toString, row(2).toString)))
      .sortBy(_._1).reverse
      .take(max)
  }
}