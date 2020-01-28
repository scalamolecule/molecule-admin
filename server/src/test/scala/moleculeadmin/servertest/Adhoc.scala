package moleculeadmin.servertest
import ammonite.ops.read
import db.admin.dsl.moleculeAdmin.{meta_Db, user_ColSetting}
import db.core.dsl.coreTest._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.servertest.schema.withPartitions.PartitionSetup
import moleculeadmin.shared.testdata.{CoreSchema, ExampleData, mBrainzSchema}
import moleculeadmin.shared.util.HelpersAdmin
import org.specs2.mutable._
import scala.languageFeature.implicitConversions._
import ammonite.ops._
import db.core.schema.CoreTestSchema
import molecule.ast.model.NoValue
import molecule.ast.transactionModel.Add
import moleculeadmin.server.QueryBackend
import moleculeadmin.server.utils.DateStrLocal
import moleculeadmin.servertest.ResetDbs.protocol
import scala.collection.mutable.ListBuffer
import utest._


object Adhoc extends TestSuite
  with HelpersAdmin
  with ExampleData
  //  with TreeSchema
  with mBrainzSchema
  with DateStrLocal {

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("Adhoc") {

      implicit val conn = recreateDbFrom(CoreTestSchema, "localhost:4334/CoreTest", protocol)
      //    implicit val conn = Conn(base + "/MoleculeAdmin")
      //    implicit val conn = Conn(base + "/mbrainz-1968-1973")
      //        implicit val conn = Conn(base + "/CoreTest")

      Ns.int(1).save
      Ns.int.get ==> List(1)
    }
  }
}