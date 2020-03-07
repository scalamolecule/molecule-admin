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
//      implicit val conn = Conn("datomic:free://localhost:4334/Clazzig")


      //      // in-memory db
            implicit val conn = recreateDbFrom(CoreTestSchema)
      //            implicit val conn = Conn(base + "/CoreTest")

      //      val List(r1, r2) = Ref1.int1.insert(10, 11).eids
      //
      //      Ns.int.ref1 insert List(
      //        (1, r1),
      //        (2, r2),
      //        (3, r2),
      //      )
      //
      ////      Ns.int.Ref1.int1.debugGet
      ////      Ns.e.ref1(r1).debugGet
      //      Ns.e(count).ref1(r2).a.debugGet


      Ns.int(1).ref1(42L).save
      Ref1.int1(2).ref2(42L).save


      Ns.e(count).a.v(42L).debugGet

//      conn.q(
//        """[:find  (count ?a) ?c ?c_a
//          | :where [(ground 285873031511718) ?c]
//          |        [?a ?attr ?c]
//          |        [?attr :db/ident ?c1]
//          |        [(str ?c1) ?c_a]
//          |        [(molecule.util.fns/live ?c_a)]]""".stripMargin
//      ) foreach println
//
//      println("-----")

//      conn.q(
//        """[:find  (count ?a) ?c ?c_a
//          | :where [(ground 285873031511722) ?c]
//          |        [?a ?attr ?c]
//          |        [?attr :db/ident ?c1]
//          |        [(str ?c1) ?c_a]
//          |        [(molecule.util.fns/live ?c_a)]]""".stripMargin
//      ) foreach println

//      ind_Person.e(count).a.

//      println("-------------")
//
//      conn.q(
//        """[:find  (count ?a) ?c ?c_a
//          | :where [(ground 294669124533924) ?c]
//          |        [?a ?attr ?c]
//          |        [?attr :db/ident ?c1]
//          |        [(str ?c1) ?c_a]
//          |        [(molecule.util.fns/live ?c_a)]]""".stripMargin
//      ) foreach println
//
//      println("-----")
//
//      conn.q(
//        """[:find  (count ?a) ?c ?c_a
//          | :where [(ground 294669124533928) ?c]
//          |        [?a ?attr ?c]
//          |        [?attr :db/ident ?c1]
//          |        [(str ?c1) ?c_a]
//          |        [(molecule.util.fns/live ?c_a)]]""".stripMargin
//      ) foreach println


    }
  }
}