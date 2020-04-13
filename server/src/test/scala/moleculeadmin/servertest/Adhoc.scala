package moleculeadmin.servertest

import db.core.dsl.coreTest._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.server.utils.DateStrLocal
import moleculeadmin.servertest.ResetDbs.protocol
import moleculeadmin.shared.ast.schema.MetaSchema
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import moleculeadmin.shared.testdata.{CoreSchema, ExampleData}
import moleculeadmin.shared.util.HelpersAdmin
import utest._
import scala.languageFeature.implicitConversions._


object Adhoc extends TestSuite
  with HelpersAdmin
  with ExampleData
  with CoreSchema
  with DateStrLocal
  with ModelOps
  with ColOps {

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("Adhoc") {

      //      implicit val conn = recreateDbFrom(CoreTestSchema, "localhost:4334/CoreTest", protocol)
      //      implicit val conn = Conn(base + "/MoleculeAdmin")
      //      implicit val conn = Conn(base + "/mbrainz-1968-1973")
      //      implicit val conn = Conn(base + "/Partition")
      //      implicit val conn = Conn(base + "/Partition1")
      //      implicit val conn = Conn(base + "/Partition2")
      //      recreateDbFrom(PartitionSchema, "localhost:4334/Partition", protocol)
      //      implicit val conn = Conn(base + "/Clazzig")

      //      implicit val conn = Conn(base + "/CoreTest")
      //      implicit val conn = recreateDbFrom(CoreTestSchema)



    }
  }
}