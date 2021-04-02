package moleculeadmin.servertest.schema.namespace

import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import db.migration.dsl.partition._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object NamespaceDelete extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Non-valid") {
      val ps = new PartitionSetup
      import ps._

      deleteNamespace(partitionMetaSchema, "qqq", "b", "Bb") ==> Left(
        "Couldn't find database `qqq`."
      )

      deleteNamespace(partitionMetaSchema, "Partition", "z", "Bb") ==> Left(
        "Couldn't find partition `z` in database `Partition`."
      )

      deleteNamespace(partitionMetaSchema, "Partition", "b", "Bz") ==> Left(
        "Couldn't find namespace `Bz` in partition `b` in database `Partition`."
      )
    }


    test("With no attributes") {
      val ps = new PartitionSetup
      import ps._

      deleteNamespace(partitionMetaSchema, "Partition", "a", "Aa") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List()),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List()),
        ))
      )

      read ! partitionDefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object PartitionDefinition {
          |
          |  object a {
          |
          |  }
          |
          |  object b {
          |
          |    trait Bb {
          |      val bb1 = oneInt
          |      val bb2 = oneInt
          |    }
          |
          |    trait Bc {
          |      val bc1 = oneInt
          |    }
          |  }
          |
          |  object c {
          |
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.name_("a").Namespaces.name.get(moleculeAdminConn) ==> Nil
    }


    test("With attributes not asserted") {
      val ps = new PartitionSetup
      import ps._

      // Could have previously asserted values
      val e = b_Bb.bb1(42).save(partitionConn).eid
      retract(Seq(e))(partitionConn)

      b_Bb(e).a.v.t.op.getHistory(partitionConn).sortBy(_._3) ==> List(
        (":b_Bb/bb1", 42, 1002, true),
        (":b_Bb/bb1", 42, 1004, false)
      )


      deleteNamespace(partitionMetaSchema, "Partition", "b", "Bb") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List()),
        ))
      )

      read ! partitionDefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object PartitionDefinition {
          |
          |  object a {
          |
          |    trait Aa {
          |
          |    }
          |  }
          |
          |  object b {
          |
          |    trait Bc {
          |      val bc1 = oneInt
          |    }
          |  }
          |
          |  object c {
          |
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name.get(moleculeAdminConn) ==> List("Bc")

      // "deleted" attributes are actually "parked" with a `-` prefix to the partition/namespace name
      partitionConn.q(
        """[:find  ?a
          | :in    $ ?part
          | :where [_ :db.install/attribute ?id ?tx]
          |        [?id :db/ident ?idIdent]
          |        [(namespace ?idIdent) ?nsFull]
          |        [(.matches ^String ?nsFull "(db|db.alter|db.excise|db.install|db.part|db.sys|fressian)") ?sys]
          |        [(= ?sys false)]
          |        [(molecule.util.fns/partNs ?nsFull) ?partNs]
          |        [(first ?partNs) ?part]
          |        [(second ?partNs) ?ns]
          |        [(str ?idIdent) ?a]]
          """.stripMargin, "-b") ==> List(
        List(":-b_Bb/bb2"),
        List(":-b_Bb/bb1"),
      )

      // Partition `b` now has only namespace `Bc` ("parked" attributes are filtered out)
      Schema.part_("b").a.get(partitionConn).sorted ==> List(
        ":b_Bc/bc1",
      )

      // Generic history search will show previous values of old attributes
      b_Bb(e).a.v.t.op.getHistory(partitionConn).sortBy(_._3) ==> List(
        (":b_Bb/bb1", 42, 1002, true),
        (":b_Bb/bb1", 42, 1004, false),
      )

      // After compiling the new schema from the updated definition file,
      // the custom API of the old partition is no longer available in code
      // b_Bb.bb1.get // <-- not available after `sbt compile`
    }


    test("With asserted attributes") {
      val ps = new PartitionSetup
      import ps._

      // Namespaces with asserted attributes can't be deleted.
      // An exception is thrown showing how many entities have values asserted for each attribute.

      // 1 value asserted
      b_Bb.bb1(42).save(partitionConn)

      // Attribute `bb1` is asserted once, so namespace `Bb` can't be deleted
      deleteNamespace(partitionMetaSchema, "Partition", "b", "Bb") ==> Left(
        "Successfully rolled back from error: " +
          "Couldn't delete namespace `Bb` in partition `b` in database `Partition` having attributes with values. Please delete values first."
      )
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}
