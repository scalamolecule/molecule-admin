package moleculeadmin.clienttest
import java.time.{Instant, LocalDateTime, ZoneId}
import java.util.{Date, TimeZone}
import molecule.util.DateHandling
import moleculeadmin.shared.Shared
import utest._

// sbt> moleculeAdminJS/test
// sbt> moleculeAdminJS/testOnly -- moleculeadmin.clienttest.TestJS

object TestJS extends TestSuite with DateHandling {

//  val d1 = new Date("2001-01-01")
//  val d2 = new Date("2002-07-01")
//
//    lazy val zone           : ZoneId     = ZoneId.of(TimeZone.getDefault.getID)
//
//
//  def daylight(ms: Long): Int = {
//    if (zone.getRules.isDaylightSavings(Instant.ofEpochMilli(ms)))
//      60 * 60 * 1000
//    else
//      0
//  }


  val tests = Tests {
    test("test") {
      println("a", "b")
      Shared.confirm("Test js")
    }


    test("moleculeadmin/client/scalafiddle") {



//      val zdt1 = LocalDateTime.of(2001, 1, 1, 0, 0).atZone(zone)
//      val zdt2 = LocalDateTime.of(2002, 7, 1, 0, 0).atZone(zone)
//
//      println(zdt1)
//      println(zdt2)
////      val isDaylight = zone.getRules.isDaylightSavings(zdt1.toInstant)
//      println(zone.getRules.isDaylightSavings(zdt1.toInstant))
//      println(zone.getRules.isDaylightSavings(zdt2.toInstant))
//
//      println(zone.getRules.getDaylightSavings(zdt1.toInstant))
//      println(zone.getRules.getDaylightSavings(zdt2.toInstant))



//      def mkDate(month: Int): Date = {
//        val inst = LocalDateTime.of(2001, month, 1, 15, 0).atZone(zone).toInstant
//        val ms   = inst.getEpochSecond * 1000 + inst.getNano / 1000000
//        new Date(ms - daylight(ms))
//      }
//      val d3 = mkDate(3)
//      val d7 = mkDate(7)
//
//      println(date1)
////      println(d2)
//      println(d3)
//      println(d7)


      Seq(
        "2019-01-11 15:00",
        "2019-07-17 15:00"
      ).foreach{d =>
        println(d)
        println(str2date(d))
        println(date2str(str2date(d)))
      }
    }

  }
}