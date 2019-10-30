package moleculeadmin.servertest
import java.time.ZoneId
import java.util.TimeZone
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema}
import moleculeadmin.shared.util.HelpersAdmin
import moleculeadmin.sharedtest2.util.DateTransformation._
import utest._


object Adhoc extends TestSuite
  with TreeSchema with HelpersAdmin with ExampleData {

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("Adhoc") {

      1 ==> 2
      //      implicit val conn = Conn(base + "/CoreTest")

    }
  }
}
