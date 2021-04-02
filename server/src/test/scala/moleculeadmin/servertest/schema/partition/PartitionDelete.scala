package moleculeadmin.servertest.schema.partition

import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import db.migration.dsl.partition._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object PartitionDelete extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Basic validation") {
      val ps = new PartitionSetup
      import ps._

      deletePartition(partitionMetaSchema, "qqq", "c") ==> Left(
        "Couldn't find database `qqq`."
      )

      deletePartition(partitionMetaSchema, "Partition", "z") ==> Left(
        "Couldn't find partition `z` in database `Partition`."
      )
    }


    test("With no namespaces") {
      val ps = new PartitionSetup
      import ps._

      deletePartition(partitionMetaSchema, "Partition", "c") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
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
          |    trait Bb {
          |      val bb1 = oneInt
          |      val bb2 = oneInt
          |    }
          |
          |    trait Bc {
          |      val bc1 = oneInt
          |    }
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.name.get(moleculeAdminConn) ==> List("a", "b")

      getPartitions(partitionConn) ==> List("a", "b")
    }


    test("With no attributes") {
      val ps = new PartitionSetup
      import ps._

      deletePartition(partitionMetaSchema, "Partition", "a") ==> Right(
        MetaSchema(List(
          MetaPart(1, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(2, "c", None, None, List()),
        ))
      )

      read ! partitionDefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object PartitionDefinition {
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

      meta_Db.name_("Partition").Partitions.name.get(moleculeAdminConn) ==> List("b", "c")

      getPartitions(partitionConn) ==> List("b", "c")
    }


    test("With attributes without data") {
      val ps = new PartitionSetup
      import ps._

      deletePartition(partitionMetaSchema, "Partition", "b") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "c", None, None, List()),
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
          |  object c {
          |
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.name.get(moleculeAdminConn) ==> List("a", "c")

      getPartitions(partitionConn) ==> List("a", "c")
    }


    test("With attributes with data") {
      val ps = new PartitionSetup
      import ps._

      // Attributes in partition have values
      b_Bb.bb1(42).save(partitionConn).eid

      deletePartition(partitionMetaSchema, "Partition", "b") ==> Left(
        "Successfully rolled back from error: " +
          "Couldn't delete partition `b` in database `Partition` having attributes with values. Please delete values first."
      )

      // Successfully rolled back (partition `b` still exists)

      // meta
      meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
        (1, "a"),
        (2, "b"),
        (3, "c"),
      )

      // live
      getPartitions(partitionConn) ==> List("a", "b", "c")
    }


    test("With retracted attributes") {
      val ps = new PartitionSetup
      import ps._

      // Could have previously asserted values
      val e = b_Bb.bb1(42).save(partitionConn).eid
      retract(Seq(e))(partitionConn)


      b_Bb(e).a.v.t.op.getHistory(partitionConn).sortBy(_._3) ==> List(
        (":b_Bb/bb1", 42, 1002, true),
        (":b_Bb/bb1", 42, 1004, false)
      )

      deletePartition(partitionMetaSchema, "Partition", "b") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "c", None, None, List()),
        ))
      )

      // Def file
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
          |  object c {
          |
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.name.get(moleculeAdminConn) ==> List("a", "c")

      // "deleted" attributes are actually "parked" with a `-` prefix to the partition/namespace name
      // and filtered out when querying for current attributes
      Schema.part_("b").a.get(partitionConn).sorted ==> Nil

      // Generic history search will show previous values where both
      // the old and new ident of the attribute will point to the same entity
      b_Bb(e).a.v.t.op.getHistory(partitionConn).sortBy(_._3) ==> List(
        (":b_Bb/bb1", 42, 1002, true),
        (":b_Bb/bb1", 42, 1004, false),
      )

      // After compiling the new schema from the updated definition file,
      // the custom API of the old partition is no longer available in code
      // b_Bb.bb1.get // <-- not available after `sbt compile`

      // Partition entity has been retracted in live db
      getPartitions(partitionConn) ==> List("a", "c")

      // No live partitions with attributes defined
      Schema.part.get(partitionConn).sorted ==> Nil
    }


    test("With asserted attributes") {
      val ps = new PartitionSetup
      import ps._

      // Partitions with asserted attributes can't be deleted.
      // An exception is thrown showing how many entities have values asserted for each attribute.

      // 1 value asserted
      b_Bb.bb1(42).save(partitionConn)

      // Attribute `bb1` is asserted once, so partition `b` can't be deleted
      deletePartition(partitionMetaSchema, "Partition", "b") ==> Left(
        "Successfully rolled back from error: " +
          "Couldn't delete partition `b` in database `Partition` having attributes with values. Please delete values first."
      )
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}
