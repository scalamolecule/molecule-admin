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
import molecule.api.core.recreateDbFrom
import molecule.ast.model.{Atom, Bond, Fn, NoValue, VarValue}
import molecule.ast.query.{Funct, NoBinding}
import molecule.ast.transactionModel.Add
import moleculeadmin.server.QueryBackend
import moleculeadmin.server.utils.DateStrLocal
import moleculeadmin.servertest.ResetDbs.protocol
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import moleculeadmin.shared.ops.transform.Molecule2Model
import scala.collection.mutable.ListBuffer
import utest._
import scala.collection.immutable
import scala.util.Random


object Adhoc extends TestSuite
  with HelpersAdmin
  with ExampleData
  //  with TreeSchema
  //  with mBrainzSchema
  with CoreSchema
  with DateStrLocal
  with ModelOps
  with ColOps {

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("Adhoc") {

      //      implicit val conn = recreateDbFrom(CoreTestSchema, "localhost:4334/CoreTest", protocol)
      //    implicit val conn = Conn(base + "/MoleculeAdmin")
      //    implicit val conn = Conn(base + "/mbrainz-1968-1973")

      //      // in-memory db
      implicit val conn = recreateDbFrom(CoreTestSchema)
      //      implicit val conn = Conn(base + "/CoreTest")


    }
  }
}