package moleculeadmin.servertest
import java.time.ZoneId
import java.util.TimeZone
import db.core.dsl.coreTest.Ns
import molecule.facade.Conn
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema}
import moleculeadmin.shared.util.HelpersAdmin
import moleculeadmin.sharedtest2.util.DateTransformation._
import utest._
import molecule.api.out10._


object Adhoc extends TestSuite
  with TreeSchema with HelpersAdmin with ExampleData {

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("Adhoc") {

//      implicit val conn = Conn(base + "/CoreTest")
      implicit val conn = Conn(base + "/mbrainz-1968-1973")

//      val eid = 716881581322888L
      val eid = 17592186072911L

      Schema.a.get.sorted foreach println

      println(Schema.a(":Release/country").get)
      println(Schema.a(":country/US").get)


      println(eid.touchQuoted)

//      Ns(eid).t.tx.txInstant.op.a.v.getHistory.sortBy(_._5) foreach println

    }
  }
}
