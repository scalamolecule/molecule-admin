package moleculeadmin.sharedtest.ops.query

import moleculeadmin.shared.ast.query.Col
import molecule.ast.model._
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object Model2Cols extends TestSuite with TreeSchema with ColOps {

  val tests = Tests {

    test("e") {
      getCols(List(Generic("Ns", "e", "datom", EntValue))) ==> List(
        Col(0,0, "Ns", "Ns", "e", "datom", "double", 1, false, Nil, "", "", "", 0)
      )
    }

    test("e") {
      val m1 = List(
        Generic("ind_Person", "e", "datom", EntValue),
        Atom("ind_Person", "name", "String", 1, VarValue, None, Seq(), Seq()),
        Bond("ind_Person", "nationality", "loc_Country", 1, Seq()),
        Generic("loc_Country", "e", "datom", EntValue))

      getCols(m1) ==> List(
        Col(0, 0, "ind_Person", "ind_Person", "e", "datom", "double", 1, false, Nil, "", "", "", 0),
        Col(1, 0, "ind_Person", "ind_Person", "name", "String", "string", 1, false, Nil, "", "", "", 0),
        Col(2, 1, "Nationality", "loc_Country", "e", "datom", "double", 1, false, Nil, "", "", "", 0))

    }
  }
}