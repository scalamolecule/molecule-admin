package moleculeadmin.servertest.schema.attribute

import java.util
import ammonite.ops._
import datomic.Util.list
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AttributeDelete extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("With data") {
      val ps = new PartitionSetup
      import ps._

      // Assert value for attribute `bb1`
      partitionConn.transact(list(list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 42.asInstanceOf[Object])).asInstanceOf[util.List[AnyRef]])
      partitionConn.q("[:find ?value :where [_ :b_Bb/bb1 ?value]]") ==> List(List(42))

      // Can't delete partition with attribute `bb1` having asserted value
      deleteAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1") ==> Left(
        "Can't delete attribute `bb1` asserted with 1 entities. Please retract or transfer attribute value(s) first."
      )

      // Successfully rolled back (attribute `bb1` unchanged)

      // client
      getMetaSchema("Partition") ==> partitionMetaSchema

      // def file
      read ! partitionDefFilePath ==> partitionDefFile

      // meta
      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").pos.card.tpe.get(moleculeAdminConn) ==> List((1, 1, "Int"))

      // live schema
      Schema.attr("bb1").card.tpe.get(partitionConn) ==> List(("bb1", "one", "long"))

      // Data intact
      partitionConn.q("[:find ?value :where [_ :b_Bb/bb1 ?value]]") ==> List(List(42))
    }

    test("Without data") {
      val ps = new PartitionSetup
      import ps._

      // Deleting attribute without value is ok
      deleteAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb2") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List())
        ))
      )
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}