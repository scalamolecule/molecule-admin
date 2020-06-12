package moleculeadmin.servertest.schema.partition

import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object PartitionUpdate extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Basic validation") {
      val ps = new PartitionSetup
      import ps._

      updatePartition(partitionMetaSchema, "Partition", "a", "") ==> Left(
        "Empty partition name."
      )

      updatePartition(partitionMetaSchema, "Partition", "a", "tx") ==> Left(
        "Partition can't have reserved name `tx` or `db`."
      )

      updatePartition(partitionMetaSchema, "Partition", "a", "MyPartition") ==> Left(
        "Partition name should start with lowercase letter and match `[a-z][a-zA-Z0-9]*`."
      )

      updatePartition(partitionMetaSchema, "Partition", "a", "my_partition") ==> Left(
        "Partition name should match `[a-z][a-zA-Z0-9]*`."
      )

      updatePartition(partitionMetaSchema, "Partition", "a", "b") ==> Left(
        "Partition `b` already exists in database `Partition`. Please choose another partition name."
      )

      updatePartition(partitionMetaSchema, "qqq", "a", "x") ==> Left(
        "Couldn't find database `qqq`."
      )

      updatePartition(partitionMetaSchema, "Partition", "z", "x") ==> Left(
        "Couldn't find partition `z` in database `Partition`."
      )
    }


    test("Order") {

      test("No change") {
        val ps = new PartitionSetup
        import ps._

        // Ordering of partitions is only for representation and has
        // no impact on the live db. We therefore don't test the live db here.

        // No change
        updatePartition(partitionMetaSchema, "Partition", "a", "a", None, 0) ==> Right(partitionMetaSchema)
        read ! partitionDefFilePath ==> partitionDefFile

        updatePartition(partitionMetaSchema, "Partition", "a", "a") ==> Right(partitionMetaSchema)
        read ! partitionDefFilePath ==> partitionDefFile

        // No change - `a` already first
        updatePartition(partitionMetaSchema, "Partition", "a", "a", None, 1) ==> Right(partitionMetaSchema)
        read ! partitionDefFilePath ==> partitionDefFile
      }


      test("1 -> 2") {
        val ps = new PartitionSetup
        import ps._


        updatePartition(partitionMetaSchema, "Partition", "a", "a", None, 2) ==> Right(
          MetaSchema(List(
            Part(1, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(2, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(3, "c", None, None, List())
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

        meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "b"),
          (2, "a"),
          (3, "c")
        )
      }


      test("1 -> 3") {
        val ps = new PartitionSetup
        import ps._

        updatePartition(partitionMetaSchema, "Partition", "a", "a", None, 3) ==> Right(
          MetaSchema(List(
            Part(1, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(2, "c", None, None, List()),
            Part(3, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List())))
          ))
        )

        meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "b"),
          (2, "c"),
          (3, "a")
        )
      }


      test("2 -> 1") {
        val ps = new PartitionSetup
        import ps._

        updatePartition(partitionMetaSchema, "Partition", "b", "b", None, 1) ==> Right(
          MetaSchema(List(
            Part(1, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(2, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(3, "c", None, None, List())
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

        meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "b"),
          (2, "a"),
          (3, "c")
        )
      }


      test("2 -> 3") {
        val ps = new PartitionSetup
        import ps._

        updatePartition(partitionMetaSchema, "Partition", "b", "b", None, 3) ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "c", None, None, List()),
            Part(3, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List())))))
          ))
        )

        meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "a"),
          (2, "c"),
          (3, "b")
        )
      }


      test("3 -> 1") {
        val ps = new PartitionSetup
        import ps._

        updatePartition(partitionMetaSchema, "Partition", "c", "c", None, 1) ==> Right(
          MetaSchema(List(
            Part(1, "c", None, None, List()),
            Part(2, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(3, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List())))))
          ))
        )

        read ! partitionDefFilePath ==>
          """package db.migration.schema
            |import molecule.schema.definition._
            |
            |@InOut(0, 5)
            |object PartitionDefinition {
            |
            |  object c {
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
            |}
            |""".stripMargin

        meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "c"),
          (2, "a"),
          (3, "b")
        )
      }
    }


    test("Description") {
      val ps = new PartitionSetup
      import ps._

      // Add description
      updatePartition(partitionMetaSchema, "Partition", "a", "a", Some("description"), 1) ==> Right(
        MetaSchema(List(
          Part(1, "a", Some("description"), None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
        ))
      )

      read ! partitionDefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object PartitionDefinition {
          |
          |
          |  // description -------------------------------------------------------------------------------
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

      meta_Db.name_("Partition").Partitions.name_("a").descr.get(moleculeAdminConn) ==> List("description")

      // Change description
      updatePartition(partitionMetaSchema, "Partition", "a", "a", Some("other description")) ==> Right(
        MetaSchema(List(
          Part(1, "a", Some("other description"), None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
        ))
      )

      read ! partitionDefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object PartitionDefinition {
          |
          |
          |  // other description -------------------------------------------------------------------------
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

      meta_Db.name_("Partition").Partitions.name_("a").descr.get(moleculeAdminConn) ==> List("other description")


      // Remove description with None
      updatePartition(partitionMetaSchema, "Partition", "a", "a", None) ==> Right(
        MetaSchema(List(
          Part(1, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
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
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.name("a").descr$.get(moleculeAdminConn) ==> List(("a", None))

      // Applying empty text has same effect as applying None
      updatePartition(partitionMetaSchema, "Partition", "a", "a", Some("")) ==> Right(
        MetaSchema(List(
          Part(1, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
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
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.name("a").descr$.get(moleculeAdminConn) ==> List(("a", None))
    }


    test("Name") {

      test("With nss") {
        val ps = new PartitionSetup
        import ps._

        updatePartition(partitionMetaSchema, "Partition", "a", "x") ==> Right(
          MetaSchema(List(
            Part(1, "x", None, None, List(
              Ns(1, "Aa", "x_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List())
          ))
        )

        meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "x"),
          (2, "b"),
          (3, "c")
        )

        getPartitions(partitionConn) ==> List("b", "c", "x")
      }


      test("With nss and attrs") {
        val ps = new PartitionSetup
        import ps._

        updatePartition(partitionMetaSchema, "Partition", "b", "x") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "x", None, None, List(
              Ns(1, "Bb", "x_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "x_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List())
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
            |  object x {
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
          (1, "a"),
          (2, "x"),
          (3, "c")
        )

        Schema.part_("x").a.get(partitionConn).sorted ==> List(
          ":x_Bb/bb1",
          ":x_Bb/bb2",
          ":x_Bc/bc1"
        )

        getPartitions(partitionConn) ==> List("a", "c", "x")
      }


      test("Without nss") {
        val ps = new PartitionSetup
        import ps._

        updatePartition(partitionMetaSchema, "Partition", "c", "x") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "x", None, None, List())
          ))
        )

        meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "a"),
          (2, "b"),
          (3, "x")
        )

        getPartitions(partitionConn) ==> List("a", "b", "x")
      }
    }


    test("All") {
      val ps = new PartitionSetup
      import ps._

      updatePartition(partitionMetaSchema, "Partition", "b", "x", Some("description"), 1) ==> Right(
        MetaSchema(List(
          Part(1, "x", Some("description"), None, List(
            Ns(1, "Bb", "x_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "x_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(2, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(3, "c", None, None, List())
        ))
      )

      read ! partitionDefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object PartitionDefinition {
          |
          |
          |  // description -------------------------------------------------------------------------------
          |
          |  object x {
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

      meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
        (1, "x"),
        (2, "a"),
        (3, "c")
      )

      Schema.part_("x").a.get(partitionConn).sorted ==> List(
        ":x_Bb/bb1",
        ":x_Bb/bb2",
        ":x_Bc/bc1"
      )

      getPartitions(partitionConn) ==> List("a", "c", "x")
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}
