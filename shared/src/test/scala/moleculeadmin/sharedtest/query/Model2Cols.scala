package moleculeadmin.sharedtest.query

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

    test("ref") {
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

    test("ref") {
      val m1 = List(
        Generic("music_Term", "e", "datom", EntValue),
        Bond("music_Term", "names", "music_TermName", 2, Seq()),
        Generic("music_TermName", "e", "datom", EntValue),
        Atom("music_TermName", "lang", "String", 1, Eq(Seq("da")), None, Seq(), Seq()),
        Atom("music_TermName", "name", "String", 1, VarValue, None, Seq(), Seq()),
        Atom("music_TermName", "namePl$", "String", 1, VarValue, None, Seq(), Seq()),
        Atom("music_TermName", "name2$", "String", 1, VarValue, None, Seq(), Seq()),
        Atom("music_TermName", "name2Pl$", "String", 1, VarValue, None, Seq(), Seq()))

      getCols(m1) ==> List(
        Col(0, 0, "music_Term", "music_Term", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(1, 1, "Names", "music_TermName", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(2, 1, "music_TermName", "music_TermName", "lang", "String", "string", 1, false, Seq(), "", "= da", "", 0, ""),
        Col(3, 1, "music_TermName", "music_TermName", "name", "String", "string", 1, false, Seq(), "", "", "", 0, ""),
        Col(4, 1, "music_TermName", "music_TermName", "namePl$", "String", "string", 1, true, Seq(), "", "", "", 0, ""),
        Col(5, 1, "music_TermName", "music_TermName", "name2$", "String", "string", 1, true, Seq(), "", "", "", 0, ""),
        Col(6, 1, "music_TermName", "music_TermName", "name2Pl$", "String", "string", 1, true, Seq(), "", "", "", 0, ""))

    }
  }
}