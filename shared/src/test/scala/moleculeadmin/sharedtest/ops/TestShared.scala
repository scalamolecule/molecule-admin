package moleculeadmin.sharedtest.ops
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema}
import moleculeadmin.shared.util.HelpersAdmin
import utest._


object TestShared extends TestSuite
  with TreeSchema with HelpersAdmin with ExampleData {

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("TestShared") {

      1 ==> 2
      //      implicit val conn = Conn(base + "/CoreTest")

    }
  }
}