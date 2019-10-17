package moleculeadmin.servertest

import moleculeadmin.shared.lib.moleculeExtras.HelpersAdmin
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema}
import moleculeadmin.shared.util.DateHandling
import utest._


object Adhoc extends TestSuite
  with TreeSchema with HelpersAdmin with DateHandling with ExampleData {

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("Adhoc") {

      //      implicit val conn = Conn(base + "/CoreTest")


    }
  }
}
