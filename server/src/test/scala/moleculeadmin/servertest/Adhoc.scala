package moleculeadmin.servertest

import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.server.utils.DateStrLocal
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import moleculeadmin.shared.testdata.{CoreSchema, ExampleData}
import moleculeadmin.shared.util.HelpersAdmin
import utest._
import scala.languageFeature.implicitConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import boopickle.Default._


object Adhoc extends TestSuite
  with SchemaResets
  with HelpersAdmin
  with ExampleData
  with CoreSchema
  with DateStrLocal
  with ModelOps
  with ColOps {


  val tests = Tests {

    test("CoreTest") {
      //      implicit val conn = recreateDbFrom(CoreTestSchema, host + "/CoreTest", protocol)
      implicit val conn = Conn(base + "/CoreTest")
      import db.core.dsl.coreTest._

    }


    test("MoleculeAdmin") {
      implicit val conn = Conn(base + "/MoleculeAdmin")
      import db.admin.dsl.moleculeAdmin._



    }


    test("mbrainz") {
      implicit val conn = Conn(base + "/mbrainz-1968-1973")
      import db.integration.dsl.mBrainz._


    }
  }

}