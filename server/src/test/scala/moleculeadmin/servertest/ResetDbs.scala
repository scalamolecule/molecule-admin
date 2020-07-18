package moleculeadmin.servertest

import molecule.facade.Conn
import moleculeadmin.shared.testdata.ExampleData
import utest._


object ResetDbs extends TestSuite with ExampleData with ResetDbsCmds {

  val tests = Tests {
//    test("Reset all") {
//      resetDbs()
//      //          populateCoreTest(Conn(base + "/CoreTest"))
//      populateCoreTestNested(Conn(base + "/CoreTest"))
//    }

    //    test("Reset all and poplulate") {
    //      resetDbs()
    //      populateCoreTest(Conn(base + "/CoreTest"))
    //      populatePartition(Conn(base + "/Partition"))
    //      populateTree(Conn(base + "/Tree"))
    //    }

    test("Reset CoreTest") {
      resetDbs(Seq("CoreTest"))
      //          populateCoreTest(Conn(base + "/CoreTest"))
      populateCoreTestNested(Conn(base + "/CoreTest"))
    }
  }

}
