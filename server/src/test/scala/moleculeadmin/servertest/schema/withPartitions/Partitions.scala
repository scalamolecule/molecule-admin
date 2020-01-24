package moleculeadmin.servertest.schema.withPartitions

import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import db.migration.dsl.partition._
import molecule.api.out10._
import moleculeadmin.servertest.ResetDbs
import moleculeadmin.shared.ast.schema._
import molecule.util.Helpers
import moleculeadmin.shared.testdata.TreeSchema
import sbtmolecule.Ast.{Definition, Namespace, Optional}
import sbtmolecule.{Ast, DefinitionParser}
import utest._


object Partitions extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Read") {
      val ps = new PartitionSetup
      import ps._

      // def file
      read ! partitionDefFilePath ==> partitionDefFile
      DefinitionParser("PartitionDefinition.scala", partitionDefFile.split("\n").toList).parse ==>
        Definition("db.migration", 0, 5, "Partition", "c", "", List(
          Namespace("a", None, "a_Aa", None, None, List()),
          Namespace("b", None, "b_Bb", None, None, List(
            Ast.Val("bb1", "bb1", "OneInt", "Int", "", "long", Seq(
              Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None),
            Ast.Val("bb2", "bb2", "OneInt", "Int", "", "long", Seq(
              Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None)
          )),
          Namespace("b", None, "b_Bc", None, None, List(
            Ast.Val("bc1", "bc1", "OneInt", "Int", "", "long", Seq(
              Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None)
          )),
          Namespace("c", None, "", None, None, Nil)))


      // meta schema
      meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
        (1, "a"),
        (2, "b"),
        (3, "c"),
      )

      // Check that nested meta schema is correct
      getMetaSchema("Partition") ==> partitionMetaSchema


      // live db
      Schema.part_("b").a.get(partitionConn).sorted ==> List(
        ":b_Bb/bb1",
        ":b_Bb/bb2",
        ":b_Bc/bc1",
      )
      getPartitions(partitionConn) ==> List("a", "b", "c")

      // Partitions with attributes
      Schema.part.get(partitionConn) ==> List("b")
    }


    test("Create") {

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
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "x", Some("Description of x"), None, List()),
            Part(3, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(4, "c", None, None, List())
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
            Part(1, "x", None, None, List()),
            Part(2, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(3, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(4, "c", None, None, List())
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
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
            Part(4, "x", None, None, List())
          ))
        )
        // ns
        createNamespace(getMetaSchema("Partition"), "Partition", "x", "Xx") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
            Part(4, "x", None, None, List(
              Ns(1, "Xx", "x_Xx", None, None, List())))
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
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
            Part(4, "x", None, None, List())
          ))
        )
        // ns
        createNamespace(getMetaSchema("Partition"), "Partition", "x", "Xx") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
            Part(4, "x", None, None, List(
              Ns(1, "Xx", "x_Xx", None, None, List())))
          ))
        )
        // attr
        createAttribute(getMetaSchema("Partition"), "Partition", "x", "Xx", "xx1", 1, "Int") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
            Part(4, "x", None, None, List(
              Ns(1, "Xx", "x_Xx", None, None, List(
                Attr(1, "xx1", 1, "Int", None, None, None, None, None, None, None, None, List())
              ))))
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
    }


    test("Update") {

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
    }


    test("Delete") {

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
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
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
            Part(1, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(2, "c", None, None, List()),
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
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "c", None, None, List()),
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
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "c", None, None, List()),
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
    }


    test("Restore") {
      ResetDbs.resetDbs()
    }
  }
}

