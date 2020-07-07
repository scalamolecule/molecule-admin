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
import db.core.dsl.coreTest._


object Adhoc extends TestSuite
  with SchemaResets
  with HelpersAdmin
  with ExampleData
  with CoreSchema
  with DateStrLocal
  with ModelOps
  with ColOps {


  val tests = Tests {

    test("Adhoc") {

      1 ==> 1

//      implicit val conn = recreateDbFrom(CoreTestSchema, host + "/CoreTest", protocol)
//      recreateDbFrom(PartitionSchema, host + "/Partition", protocol)
//      implicit val conn = Conn(uriBase + "/Partition")
//      implicit val conn = Conn(uriBase + "/Partition1")
//      implicit val conn = Conn(uriBase + "/mbrainz-1968-1973")
      implicit val conn = Conn(base + "/CoreTest")

//      installMoleculeAdminSampleDbs(Seq("Partition"))
//
//      createAttribute(partitionMetaSchema, "Partition", "a", "Aa", "", 1, "Int") ==> Left(
//        "Empty attribute name."
//      )


    }
  }
}