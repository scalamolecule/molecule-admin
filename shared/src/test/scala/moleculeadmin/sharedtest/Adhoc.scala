package moleculeadmin.sharedtest

import molecule.ast.model.{Atom, Bond, Element, EntValue, EnumVal, Eq, Fn, Generic, Model, NoValue, ReBond, VarValue}
import molecule.ops.VerifyRawModel
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.transform.Molecule2Model
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema, mBrainzSchema}
import moleculeadmin.shared.util.HelpersAdmin
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


    }
  }
}