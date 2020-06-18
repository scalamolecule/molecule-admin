package moleculeadmin.sharedtest

import java.time.ZoneOffset
import molecule.ast.model.{Atom, Bond, EntValue, EnumVal, Eq, Generic, Model, VarValue}
import molecule.ops.VerifyRawModel
import molecule.util.DateHandling
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.transform.Molecule2Model
import moleculeadmin.shared.testdata.TreeSchema
import moleculeadmin.sharedtest.query.branch.AddNsAttr.addNs
import moleculeadmin.sharedtest.query.branch.UpsertBranch.upsertBranch
import moleculeadmin.sharedtest.transform.Molecule2ModelTest.{date1, date2, date2str}
import moleculeadmin.sharedtest.util.DateTransformation.{localOffset, truncateDateStr}
import utest._
import scala.languageFeature.implicitConversions._


object Adhoc extends TestSuite
  with TreeSchema with TreeOps with DateHandling {


  val tests = Tests {

    test("Adhoc") {



    }
  }
}