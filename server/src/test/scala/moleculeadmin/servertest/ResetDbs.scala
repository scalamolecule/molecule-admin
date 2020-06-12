package moleculeadmin.servertest

import molecule.facade.Conn
import moleculeadmin.shared.testdata.ExampleData
import utest._


object ResetDbs extends TestSuite with ExampleData with ResetDbsCmds {

  val tests = Tests {
    //    test("Reset all") {
    //      resetDbs()
    //      populateCoreTest(Conn(uriBase + "/CoreTest"))
    //    }

    //    test("Reset all and poplulate") {
    //      resetDbs()
    //      populateCoreTest(Conn(uriBase + "/CoreTest"))
    //      populatePartition(Conn(uriBase + "/Partition"))
    //      populateTree(Conn(uriBase + "/Tree"))
    //    }

    //    test("Reset CoreTest") {
    //      resetDbs(Seq("CoreTest"))
    //      populateCoreTest(Conn(base + "/CoreTest"))
    //    }
  }

}
