package moleculeadmin.sharedtest

import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object Adhoc extends TestSuite
  with TreeSchema with TreeOps {

  val tests = Tests {

    test("Adhoc") {


    }
  }
}