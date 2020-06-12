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
import moleculeadmin.sharedtest.util.DateTransformation.{localOffset, truncateDateStr}
import utest._
import scala.languageFeature.implicitConversions._


object Adhoc extends TestSuite
  with TreeSchema with TreeOps with DateHandling {



  val myPlus2hourZone = ZoneOffset.of("+2")
  val plus1hourZone   = ZoneOffset.of("+1")
  val utcZone         = ZoneOffset.of("+0") // Same as ZoneOffset.UTC
  val minus1hourZone  = ZoneOffset.of("-1")

  val otherOffset = "+01:45"

  val tests = Tests {

    test("Adhoc") {

      truncateDateStr(s"2019") ==> "2019-01-01"

//      localOffset ==> 7
      truncateDateStr(s"2019-02-12 00:00 $localOffset") ==> "2019-02-12"



    }
  }
}