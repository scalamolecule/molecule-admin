package moleculeadmin.sharedtest.query.col

import moleculeadmin.shared.ast.query.Col
import molecule.ast.model._
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.testdata.TreeSchema
import moleculeadmin.sharedtest.query.Model2Cols.getCols
import utest._


object GroupableCols extends TestSuite with TreeSchema with ColOps {


  val tests = Tests {

    test("no e") {
      getGroupableCols(
        List(
          Col(0, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "", "", 0)
        )
      ) ==> List()
    }


    test("e not first") {
      getGroupableCols(
        List(
          Col(0, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "", "", 0),
          Col(1, 0, "Ns", "Ns", "e", "datom", "double", 1, false, Seq(), "", "", "", 0),
          Col(2, 0, "Ns", "Ns", "int", "Int", "double", 1, false, Seq(), "", "", "", 0)
        )
      ) ==> List()
    }


    test("e first") {
      getGroupableCols(
        List(
          Col(0, 0, "Ns", "Ns", "e", "datom", "double", 1, false, Seq(), "", "", "", 0),
          Col(1, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "", "", 0)
        )
      ) ==> List(
        Col(1, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "", "", 0)
      )
    }


    test("e not first in ref ns") {
      getGroupableCols(
        List(
          Col(0, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "", "", 0),
          Col(1, 1, "Ref1", "Ref1", "int1", "Int", "double", 1, false, Seq(), "", "", "", 0),
          Col(2, 1, "Ref1", "Ref1", "e", "datom", "double", 1, false, Seq(), "", "", "", 0))
      ) ==> List()
    }


    test("e first in ref ns") {
      getGroupableCols(
        List(
          Col(0, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "", "", 0),
          Col(1, 1, "Ref1", "Ref1", "e", "datom", "double", 1, false, Seq(), "", "", "", 0),
          Col(2, 1, "Ref1", "Ref1", "int1", "Int", "double", 1, false, Seq(), "", "", "", 0)
        )
      ) ==> List(
        Col(2, 1, "Ref1", "Ref1", "int1", "Int", "double", 1, false, Seq(), "", "", "", 0)
      )
    }

    test("two ns with eid") {
      getGroupableCols(
        List(
          Col(0, 0, "Release", "Release", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(2, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(3, 1, "Artists", "Artist", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(4, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(5, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, "")
        )
      ) ==> List(
        Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(2, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(4, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(5, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, "")
      )

      // tx Data

      getGroupableCols(
        List(
          Col(0, 0, "Release", "Release", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(2, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "txInstant", "", 0, ""),
          Col(3, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(4, 1, "Artists", "Artist", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(5, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(6, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, "")
        )
      ) ==> List(
        Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(3, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(5, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(6, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, "")
      )

      getGroupableCols(
        List(
          Col(0, 0, "Release", "Release", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(2, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(3, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "txInstant", "", 0, ""),
          Col(4, 1, "Artists", "Artist", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(5, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(6, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, "")
        )
      ) ==> List(
        Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(2, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(5, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(6, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, "")
      )

      getGroupableCols(
        List(
          Col(0, 0, "Release", "Release", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(2, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(3, 1, "Artists", "Artist", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(4, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(5, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "txInstant", "", 0, ""),
          Col(6, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, "")
        )
      ) ==> List(
        Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(2, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(4, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(6, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, "")
      )

      getGroupableCols(
        List(
          Col(0, 0, "Release", "Release", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(2, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(3, 1, "Artists", "Artist", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(4, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
          Col(5, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, ""),
          Col(6, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "txInstant", "", 0, "")
        )
      ) ==> List(
        Col(1, 0, "Release", "Release", "month", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(2, 0, "Release", "Release", "day", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(4, 1, "Artists", "Artist", "startYear", "Long", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(5, 1, "Artists", "Artist", "name", "String", "string", 1, false, Seq(), "", "", "", 0, "")
      )
    }
  }
}
