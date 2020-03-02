package moleculeadmin.sharedtest

import molecule.ast.model.{Atom, Bond, EntValue, EnumVal, Eq, Generic, Model, VarValue}
import molecule.ops.VerifyRawModel
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.transform.Molecule2Model
import moleculeadmin.shared.testdata.TreeSchema
import moleculeadmin.sharedtest.query.branch.AddNsAttr.addNs
import moleculeadmin.sharedtest.query.branch.UpsertBranch.upsertBranch
import utest._
import scala.languageFeature.implicitConversions._


object Adhoc extends TestSuite
  with TreeSchema with TreeOps {

  val tests = Tests {

    test("Adhoc") {


    }
  }
}