package moleculeadmin.sharedtest

import molecule.ast.model.{Atom, Bond, Element, EntValue, EnumVal, Eq, Fn, Generic, Model, NoValue, ReBond, VarValue}
import molecule.ops.VerifyRawModel
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.transform.Molecule2Model
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema, mBrainzSchema}
import moleculeadmin.shared.util.HelpersAdmin
import moleculeadmin.sharedtest.query.Model2Cols.getCols
import moleculeadmin.sharedtest.query.attr.SetMode.setMode
import moleculeadmin.sharedtest.query.attr.ToggleMode.{e, e_, int, int$, intEqCoTx, intEqCoTx_, int_, toggleMode, toggleNs}
import moleculeadmin.sharedtest.query.branch.AddNsAttr.addNs
import moleculeadmin.sharedtest.query.branch.Branch.isolateBranch
import utest._
import scala.languageFeature.implicitConversions._
import scala.util.Random


object Adhoc extends TestSuite
  with TreeSchema with TreeOps {

  val tests = Tests {

    test("Adhoc") {

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