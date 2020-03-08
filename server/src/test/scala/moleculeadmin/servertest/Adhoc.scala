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
import datomic.Peer
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

      Ns.ref1.debugGet
      Ns.ref1(42L).debugGet

//      val e = Ref2.int2(42).save.eid
//
//      Ns.int.ref1 insert List(
//        (1, e),
//        (2, e),
//      )
//      Ref1.int1(3).ref2(e).save


//      Ns.e(count).a.v(e).debugGet


//      Ns.e.ref1(e).debugGet

//      conn.q(
//        s"""[:find  (count ?backRef) ?attrName
//           | :where [?backRef ?attrId $e]
//           |        [?attrId :db/ident ?attrIdent]
//           |        [(str ?attrIdent) ?attrName]]""".stripMargin
//      ) foreach println
//
//      println("----------------")
//
//      val raw1  = conn.q(
//        s"""[:find  (count ?backRef) ?attrName
//           | :in $$ ?eid
//           | :where [?backRef ?attrId ?eid]
//           |        [?attrId :db/ident ?attrIdent]
//           |        [(str ?attrIdent) ?attrName]]""".stripMargin,
//        e
//      )
//      println("length " + raw1.size)
//
//      println("----------------")
//
//      val raw2  = conn.qRaw(
//        s"""[:find  (count ?backRef) ?attrName
//           | :in $$ ?eid
//           | :where [?backRef ?attrId ?eid]
//           |        [?attrId :db/ident ?attrIdent]
//           |        [(str ?attrIdent) ?attrName]]""".stripMargin,
//        e
//      )
//      println("length " + raw2.size)
//
//      println("----------------")
//
//      val raw3  = Peer.q(
//        s"""[:find  ?attrName (count ?backRef)
//           | :in $$ ?eid
//           | :where [?backRef ?attrId ?eid]
//           |        [?attrId :db/ident ?attrIdent]
//           |        [(str ?attrIdent) ?attrName]]""".stripMargin,
//        conn.db,
//        e.asInstanceOf[Object]
//      )
//
//      println("length " + raw3.size)
//
//      val data = new ListBuffer[(String, Int)]
//      val it   = raw3.iterator()
//      while (it.hasNext) {
//        val row = it.next
//        data.+=((row.get(0).toString, row.get(1).toString.toInt))
//      }
//      data foreach println


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