package moleculeadmin.servertest

import java.util.Date
import db.core.dsl.coreTest._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.shared.lib.moleculeExtras.HelpersAdmin
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema}
import moleculeadmin.shared.util.DateHandling
import utest._

//import scalafiddle.client


object Adhoc extends TestSuite
  with TreeSchema with HelpersAdmin with DateHandling
  //  with ExampleData
{

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("Adhoc") {

      implicit val conn = Conn(base + "/CoreTest")

      //      Ns.dates.debugGet
      //      Ns.dateMap.debugGet
      //
      //      val d1 = Ns.dates.get.head.head
      //      val d2 = Ns.dateMapK("a").get.head
      //
      //      println(date2str(d1))
      //      println(truncateDateStrFull(date2str(d1)))
      //      println(truncateDateStr(date2str(d1)))
      //      println("---")
      //      println(dateLocal2str(d1))
      //      println(truncateDateStrFull(dateLocal2str(d1)))
      //      println(truncateDateStr(dateLocal2str(d1)))
      //      println("---")
      //      println(dateZone2str(d1))
      //      println(truncateDateStrFull(dateZone2str(d1)))
      //      println(truncateDateStr(dateZone2str(d1)))
      //
      //      println("==============")
      //      println(date2str(d2))
      //      println(truncateDateStrFull(date2str(d2)))
      //      println(truncateDateStr(date2str(d2)))
      //      println("---")
      //      println(dateLocal2str(d2))
      //      println(truncateDateStrFull(dateLocal2str(d2)))
      //      println(truncateDateStr(dateLocal2str(d2)))
      //      println("---")
      //      println(dateZone2str(d2))
      //      println(truncateDateStrFull(dateZone2str(d2)))
      //      println(truncateDateStr(dateZone2str(d2)))
      //
      //
      //
      //      Ns.int(44).date(date1).save
      //      Ns.int(55).dates(date1).save
      //      Ns.int(66).dateMap("a" -> date1).save
      //
      //      //    Ns.int(44).date(date1).debugSave
      //      //    Ns.int(55).dates(date1).debugSave
      //      //    Ns.int(66).dateMap("a" -> date1).debugSave
      //
      //      val d1 = Ns.int_(44).date.get.head
      ////      val d2 = Ns.int_(55).dates.get.head.head
      ////      val d3 = Ns.int_(66).dateMap.get.head.head._2
      //
      //      //    println(date2str(date1))
      //      //    println(date2str(d1))
      //      //    println(date2str(d2))
      //      //    println(date2str(d3))
      //
      //      Ns.int(44).date.debugGet
      //      Ns.int(55).dates.debugGet
      //      Ns.int(66).dateMap.debugGet

      //      val d2 = str2dateLocal("2001")
      val d1 = str2date("2001")
      val d2 = str2date("2001-01-01T00:00:00.000+00:00")
      val d3 = str2date("2001-01-01T00:00:00.000+01:00")
      val d4 = str2date("2001-01-01T01:00:00.000+01:00")

      println(date2str(d1))
      println(date2str(d2))
      println(date2str(d3))
      println(date2str(d4))
      println("------")
      println(dateZone2str(d1))
      println(dateZone2str(d2))
      println(dateZone2str(d3))
      println(dateZone2str(d4))

      //      val d1 = str2dateLocal("2001")
      //      val d2 = str2dateLocal("2001-01-01T00:00:00.000+00:00")
      //      val d3 = str2dateLocal("2001-01-01T00:00:00.000+01:00")
      //      val d4 = str2dateLocal("2001-01-01T01:00:00.000+01:00")
      //
      //      println(date2str(d1))
      //      println(date2str(d2))
      //      println(date2str(d3))
      //      println(date2str(d4))
      //
      //      println("------")
      //      println(dateLocal2str(d1))
      //      println(dateLocal2str(d2))
      //      println(dateLocal2str(d3))
      //      println(dateLocal2str(d4))

      //      val d11 = new Date("2001")
      //      val d12 = new Date("2001-01-01T00:00:00.000+00:00")
      //      val d13 = new Date("2001-01-01T00:00:00.000+01:00")
      //      val d14 = new Date("2001-01-01T01:00:00.000+01:00")
      //      val d14 = new Date("2001-01-01")
      //      val d14 = new Date()

      //      println(date2str(d11))
      //      println(date2str(d12))
      //      println(date2str(d13))
      //      println(date2str(d14))

    }
  }
}
