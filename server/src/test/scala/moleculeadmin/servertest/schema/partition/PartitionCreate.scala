package moleculeadmin.servertest.schema.partition

import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object PartitionCreate extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Basic validation") {
      val ps = new PartitionSetup
      import ps._

      createPartition(partitionMetaSchema, "Partition", "") ==> Left(
        "Empty partition name."
      )

      createPartition(partitionMetaSchema, "Partition", "tx") ==> Left(
        "Partition can't have reserved name `tx` or `db`."
      )

      createPartition(partitionMetaSchema, "Partition", "B") ==> Left(
        "Partition name should start with lowercase letter and match `[a-z][a-zA-Z0-9]*`."
      )

      createPartition(partitionMetaSchema, "Partition", "b-x") ==> Left(
        "Partition name should match `[a-z][a-zA-Z0-9]*`."
      )

      createPartition(partitionMetaSchema, "Partition", "b") ==> Left(
        "Partition `b` already exists in database `Partition`. Please choose another partition name."
      )

      createPartition(partitionMetaSchema, "qqq", "b") ==> Left(
        "Couldn't find database `qqq`."
      )
    }


    test("Partition only (with descr + pos)") {
      val ps = new PartitionSetup
      import ps._

      // Client schema
      createPartition(partitionMetaSchema, "Partition", "x", Some("Description of x"), 2) ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "x", Some("Description of x"), None, List()),
          MetaPart(3, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(4, "c", None, None, List())
        ))
      )

      // def file has partition prepared, here with description and positioned between partitions `a` and `b`
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
          |
          |  // Description of x --------------------------------------------------------------------------
          |
          |  object x {
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

      // meta schema (saving meta partition for possible ns/attr additions)
      meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
        (1, "a"),
        (2, "x"),
        (3, "b"),
        (4, "c")
      )

      // live schema
      getPartitions(partitionConn) ==> List("a", "b", "c", "x")

      // Partitions having defined attributes
      Schema.part.get(partitionConn) ==> List("b")
    }


    test("Partition only, first") {
      val ps = new PartitionSetup
      import ps._

      createPartition(partitionMetaSchema, "Partition", "x", None, 1) ==> Right(
        MetaSchema(List(
          MetaPart(1, "x", None, None, List()),
          MetaPart(2, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(3, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(4, "c", None, None, List())
        ))
      )

      read ! partitionDefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object PartitionDefinition {
          |
          |  object x {
          |
          |  }
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
          |
          |  object c {
          |
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
        (1, "x"),
        (2, "a"),
        (3, "b"),
        (4, "c")
      )

      getPartitions(partitionConn) ==> List("a", "b", "c", "x")

      Schema.part.get(partitionConn) ==> List("b")
    }


    test("Partition + namespace") {
      val ps = new PartitionSetup
      import ps._


      // part
      createPartition(partitionMetaSchema, "Partition", "x") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List()),
          MetaPart(4, "x", None, None, List())
        ))
      )
      // ns
      createNamespace(getMetaSchema("Partition"), "Partition", "x", "Xx") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List()),
          MetaPart(4, "x", None, None, List(
            MetaNs(1, "Xx", "x_Xx", None, None, List())))
        ))
      )

      // def file has partition and ns prepared
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
          |
          |  object c {
          |
          |  }
          |
          |  object x {
          |
          |    trait Xx {
          |
          |    }
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
        (1, "a"),
        (2, "b"),
        (3, "c"),
        (4, "x")
      )

      getPartitions(partitionConn) ==> List("a", "b", "c", "x")

      Schema.part.get(partitionConn) ==> List("b")
    }


    test("Partition + namespace + attribute") {
      val ps = new PartitionSetup
      import ps._

      // part
      createPartition(partitionMetaSchema, "Partition", "x") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List()),
          MetaPart(4, "x", None, None, List())
        ))
      )
      // ns
      createNamespace(getMetaSchema("Partition"), "Partition", "x", "Xx") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List()),
          MetaPart(4, "x", None, None, List(
            MetaNs(1, "Xx", "x_Xx", None, None, List())))
        ))
      )
      // attr
      createAttribute(getMetaSchema("Partition"), "Partition", "x", "Xx", "xx1", 1, "Int") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List()),
          MetaPart(4, "x", None, None, List(
            MetaNs(1, "Xx", "x_Xx", None, None, List(
              MetaAttr(1, "xx1", 1, "Int", Nil, None, Seq("indexed"), None, None, None, None, None, List()
            )))))
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
          |
          |  object c {
          |
          |  }
          |
          |  object x {
          |
          |    trait Xx {
          |      val xx1 = oneInt
          |    }
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
        (1, "a"),
        (2, "b"),
        (3, "c"),
        (4, "x")
      )

      getPartitions(partitionConn) ==> List("a", "b", "c", "x")

      Schema.part.get(partitionConn) ==> List("b", "x")
    }


    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}
