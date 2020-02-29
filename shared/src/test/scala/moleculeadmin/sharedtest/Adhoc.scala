package moleculeadmin.sharedtest

import molecule.ast.model.{Atom, EnumVal, Eq, VarValue}
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.transform.Molecule2Model
import moleculeadmin.shared.testdata.TreeSchema
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