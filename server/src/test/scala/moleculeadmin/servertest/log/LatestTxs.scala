package moleculeadmin.servertest.log

import db.core.dsl.coreTest.Ns
import db.core.schema.CoreTestSchema
import molecule.api.out10.recreateDbFrom
import molecule.util.Helpers
import moleculeadmin.server.QueryBackend
import moleculeadmin.servertest.Adhoc.date2strLocal
import moleculeadmin.servertest.ResetDbs.protocol
import moleculeadmin.shared.testdata.TreeSchema
import utest.{TestSuite, Tests, test}
import scala.collection.mutable.ListBuffer
import molecule.api.out10._
import moleculeadmin.shared.api.QueryApi
import utest._


object LatestTxs extends TestSuite with QueryApi {


  val tests = Tests {
    test("tx meta data") {
      implicit val conn = recreateDbFrom(CoreTestSchema, "localhost:4334/CoreTest", protocol)

      val tx1       = Ns.long(1).save
      val List(e11) = tx1.eids

      val tx2       = Ns.long(2).Tx(Ns.str("A")).save
      val List(e21) = tx2.eids

      val tx3            = Ns.long(3).Tx(Ns.str("A").Ref1.str1("B")).save
      val List(e31, e32) = tx3.eids

      val tx4                 = Ns.long(4).Tx(Ns.str("A").Ref1.str1("B").Ref2.strs2("C")).save
      val List(e41, e42, e43) = tx4.eids

      val tx5                      = Ns.long(5).Ref1.int1(6).Tx(Ns.str("A").int(11).Ref1.str1("B").int1(22).Ref2.strs2("C").int2(33)).save
      val List(e51, e52, e53, e54) = tx5.eids


      val txData = (new QueryBackend).getLastTxs("CoreTest", 0L, Nil)
        .getOrElse(Array.empty[TxResult])


      txData(0) ==> (tx1.t, tx1.tx, date2strLocal(tx1.inst),
        ListBuffer((tx1.tx, ":db/txInstant", date2strLocal(tx1.inst), true)),
        ListBuffer((e11, ":Ns/long", "1", true))
      )

      txData(1) ==> (tx2.t, tx2.tx, date2strLocal(tx2.inst),
        ListBuffer(
          (tx2.tx, ":db/txInstant", date2strLocal(tx2.inst), true),
          (tx2.tx, ":Ns/str", "A", true)
        ),
        ListBuffer((e21, ":Ns/long", "2", true))
      )

      txData(2) ==> (tx3.t, tx3.tx, date2strLocal(tx3.inst),
        ListBuffer(
          (tx3.tx, ":db/txInstant", date2strLocal(tx3.inst), true),
          (tx3.tx, ":Ns/str", "A", true),
          (tx3.tx, ":Ns/ref1", e32.toString, true),
          (e32, ":Ref1/str1", "B", true)),
        ListBuffer(
          (e31, ":Ns/long", "3", true))
      )

      txData(3) ==> (tx4.t, tx4.tx, date2strLocal(tx4.inst),
        ListBuffer(
          (tx4.tx, ":db/txInstant", date2strLocal(tx4.inst), true),
          (tx4.tx, ":Ns/str", "A", true),
          (tx4.tx, ":Ns/ref1", e42.toString, true),
          (e42, ":Ref1/str1", "B", true),
          (e42, ":Ref1/ref2", e43.toString, true),
          (e43, ":Ref2/strs2", "C", true)),
        ListBuffer(
          (e41, ":Ns/long", "4", true))
      )

      txData(4) ==> (tx5.t, tx5.tx, date2strLocal(tx5.inst),
        ListBuffer(
          (tx5.tx, ":db/txInstant", date2strLocal(tx5.inst), true),
          (tx5.tx, ":Ns/str", "A", true),
          (tx5.tx, ":Ns/int", "11", true),
          (tx5.tx, ":Ns/ref1", e53.toString, true),
          (e53, ":Ref1/str1", "B", true),
          (e53, ":Ref1/int1", "22", true),
          (e53, ":Ref1/ref2", e54.toString, true),
          (e54, ":Ref2/strs2", "C", true),
          (e54, ":Ref2/int2", "33", true)),
        ListBuffer(
          (e51, ":Ns/long", "5", true),
          (e51, ":Ns/ref1", e52.toString, true),
          (e52, ":Ref1/int1", "6", true))
      )
    }
  }
}