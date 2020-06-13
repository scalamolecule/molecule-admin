package moleculeadmin.sharedtest.query.col

import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object ColOpsTest extends TestSuite with TreeSchema with ColOps {


  val tests = Tests {

    test("eid col index") {
      getEidColIndex(
        List(
          Col(0, 0, "Ns", "Ns", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(1, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "", "", 0, ""),
          Col(2, 1, "Ref1", "Ref1", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(3, 1, "Ref1", "Ref1", "int1", "Int", "double", 1, false, Seq(), "", "", "", 0, "")
        ),
        3, "Ref1", "Ref1"
      ) ==> 2
    }
  }
}
