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


object Namespaces extends TestSuite with TreeSchema with Helpers {

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

        createNamespace(partitionMetaSchema, "", "b", "Bx") ==> Left(
          "Empty db name."
        )

        createNamespace(partitionMetaSchema, "Partition", "", "Bx") ==> Left(
          "Empty partition name."
        )

        createNamespace(partitionMetaSchema, "Partition", "b", "") ==> Left(
          "Empty namespace name."
        )

        createNamespace(partitionMetaSchema, "Partition", "b", "bx") ==> Left(
          "Attribute name should start with uppercase letter and match `[A-Z][a-zA-Z0-9]*`."
        )

        createNamespace(partitionMetaSchema, "Partition", "b", "B-x") ==> Left(
          "Attribute name should match `[A-Z][a-zA-Z0-9]*`."
        )

        createNamespace(partitionMetaSchema, "Partition", "b", "Bc") ==> Left(
          "Attribute `Bc` already exists in partition `b` in database `Partition`. Please choose another namespace name."
        )

        createNamespace(partitionMetaSchema, "qqq", "b", "Bx") ==> Left(
          "Couldn't find database `qqq`."
        )

        createNamespace(partitionMetaSchema, "Partition", "z", "Bx") ==> Left(
          "Couldn't find partition `z` in database `Partition`."
        )
      }


      test("Attribute, descr, pos") {
        val ps = new PartitionSetup
        import ps._

        createNamespace(partitionMetaSchema, "Partition", "b", "Bx", Some("Description of Bx"), 2) ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bx", "b_Bx", Some("Description of Bx"), None, List()),
              Ns(3, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
          ))
        )

        // def file has namespace prepared, here with description and positioned between namespaces `Bb` and `Bc`
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
            |    // Description of Bx
            |    trait Bx {
            |
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
        meta_Db.name_("Partition").Partitions.name_("b").Namespaces.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "Bb"),
          (2, "Bx"),
          (3, "Bc"),
        )

        // live namespaces having defined attributes
        Schema.part_("b").ns.get(partitionConn).sorted ==> List("Bb", "Bc")
      }


      test("Attribute only, first") {
        val ps = new PartitionSetup
        import ps._

        createNamespace(partitionMetaSchema, "Partition", "b", "Bx", None, 1) ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bx", "b_Bx", None, None, List()),
              Ns(2, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(3, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
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
            |    trait Bx {
            |
            |    }
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

        meta_Db.name_("Partition").Partitions.name_("b").Namespaces.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "Bx"),
          (2, "Bb"),
          (3, "Bc"),
        )

        Schema.part_("b").ns.get(partitionConn).sorted ==> List("Bb", "Bc")
      }


      test("Attribute") {
        val ps = new PartitionSetup
        import ps._

        createNamespace(partitionMetaSchema, "Partition", "b", "Bx") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(3, "Bx", "b_Bx", None, None, List()),
            )),
            Part(3, "c", None, None, List()),
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
            |
            |    trait Bx {
            |
            |    }
            |  }
            |
            |  object c {
            |
            |  }
            |}
            |""".stripMargin

        meta_Db.name_("Partition").Partitions.name_("b").Namespaces.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "Bb"),
          (2, "Bc"),
          (3, "Bx"),
        )

        Schema.part_("b").ns.get(partitionConn).sorted ==> List("Bb", "Bc")
      }


      test("Attribute + attribute") {
        val ps = new PartitionSetup
        import ps._

        // ns
        createNamespace(partitionMetaSchema, "Partition", "b", "Bx") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(3, "Bx", "b_Bx", None, None, List()))),
            Part(3, "c", None, None, List()),
          ))
        )
        // attr
        createAttribute(getMetaSchema("Partition"), "Partition", "b", "Bx", "bx1", 1, "Int") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(3, "Bx", "b_Bx", None, None, List(
                Attr(1, "bx1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
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
            |
            |    trait Bx {
            |      val bx1 = oneInt
            |    }
            |  }
            |
            |  object c {
            |
            |  }
            |}
            |""".stripMargin

        meta_Db.name_("Partition").Partitions.name_("b").Namespaces.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
          (1, "Bb"),
          (2, "Bc"),
          (3, "Bx"),
        )

        Schema.part_("b").ns.get(partitionConn).sorted ==> List("Bb", "Bc", "Bx")
      }
    }


    test("Update") {

      test("Basic validation") {
        val ps = new PartitionSetup
        import ps._

        updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "") ==> Left(
          "Empty new namespace name."
        )

        updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "bx") ==> Left(
          "Attribute name should start with uppercase letter and match `[A-Z][a-zA-Z0-9]*`."
        )

        updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "B-x") ==> Left(
          "Attribute name should match `[A-Z][a-zA-Z0-9]*`."
        )

        updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bc") ==> Left(
          "Attribute `Bc` already exists in partition `b` in database `Partition`. Please choose another namespace name."
        )

        updateNamespace(partitionMetaSchema, "qqq", "b", "Bb", "Bx") ==> Left(
          "Couldn't find database `qqq`."
        )

        updateNamespace(partitionMetaSchema, "Partition", "z", "Bb", "Bx") ==> Left(
          "Couldn't find partition `z` in database `Partition`."
        )

        updateNamespace(partitionMetaSchema, "Partition", "b", "Bz", "Bx") ==> Left(
          "Couldn't find current namespace `Bz` in partition `b` in database `Partition`."
        )
      }


      test("Order") {

        test("No change") {
          val ps = new PartitionSetup
          import ps._

          // No change
          updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bb") ==> Right(partitionMetaSchema)
          read ! partitionDefFilePath ==> partitionDefFile

          // No change - `a` is already first
          updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bb", None, 1) ==> Right(partitionMetaSchema)
          read ! partitionDefFilePath ==> partitionDefFile
        }


        test("1 -> 2") {
          val ps = new PartitionSetup
          import ps._

          updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bb", None, 2) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
              Part(3, "c", None, None, List()),
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
              |
              |    trait Bb {
              |      val bb1 = oneInt
              |      val bb2 = oneInt
              |    }
              |  }
              |
              |  object c {
              |
              |  }
              |}
              |""".stripMargin

          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
            (1, "Bc"),
            (2, "Bb"),
          )
        }


        test("2 -> 1") {
          val ps = new PartitionSetup
          import ps._

          updateNamespace(partitionMetaSchema, "Partition", "b", "Bc", "Bc", None, 1) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
              Part(3, "c", None, None, List()),
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
              |
              |    trait Bb {
              |      val bb1 = oneInt
              |      val bb2 = oneInt
              |    }
              |  }
              |
              |  object c {
              |
              |  }
              |}
              |""".stripMargin

          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
            (1, "Bc"),
            (2, "Bb"),
          )
        }


        test("Description") {
          val ps = new PartitionSetup
          import ps._

          updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bb", Some("description"), 1) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", Some("description"), None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
              Part(3, "c", None, None, List()),
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
              |    // description
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

          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").descr.get(moleculeAdminConn) ==> List("description")


          // Change description
          updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bb", Some("other description")) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", Some("other description"), None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
              Part(3, "c", None, None, List()),
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
              |    // other description
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

          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").descr.get(moleculeAdminConn) ==> List("other description")


          // Remove description with None
          updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bb", None) ==> Right(
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

          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name("Bb").descr$.get(moleculeAdminConn) ==> List(("Bb", None))


          // Applying empty text has same effect as applying None
          updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bb", Some("")) ==> Right(
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

          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name("Bb").descr$.get(moleculeAdminConn) ==> List(("Bb", None))
        }


        test("Name") {
          val ps = new PartitionSetup
          import ps._

          updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bx") ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bx", "b_Bx", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
              Part(3, "c", None, None, List()),
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
              |    trait Bx {
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

          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
            (1, "Bx"),
            (2, "Bc"),
          )

          Schema.part_("b").a.get(partitionConn).sorted ==> List(
            ":b_Bc/bc1",
            ":b_Bx/bb1",
            ":b_Bx/bb2",
          )
        }


        test("Name with self-join") {
          val ps = new PartitionSetup
          import ps._

          // Add self-join
          val schema1: MetaSchema = createAttribute(partitionMetaSchema, "Partition", "b", "Bc",
            "selfJoin", 1, "ref", Nil, Some("b_Bc")).getOrElse(MetaSchema(Nil))

          schema1 ==> MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "selfJoin", 1, "ref", None, Some("b_Bc"), Some(Set("indexed")), None, None, None, None, None, List())
              )))),
            Part(3, "c", None, None, List()),
          ))

          updateNamespace(schema1, "Partition", "b", "Bc", "Bc1") ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Bc1", "b_Bc1", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "selfJoin", 1, "ref", None, Some("b_Bc1"), Some(Set("indexed")), None, None, None, None, None, List())
                )))),
              Part(3, "c", None, None, List()),
            ))
          )
        }


        test("All") {
          val ps = new PartitionSetup
          import ps._

          updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bx", Some("description"), 2) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Bx", "b_Bx", Some("description"), None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              )),
              Part(3, "c", None, None, List()),
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
              |
              |    // description
              |    trait Bx {
              |      val bb1 = oneInt
              |      val bb2 = oneInt
              |    }
              |  }
              |
              |  object c {
              |
              |  }
              |}
              |""".stripMargin

          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
            (1, "Bc"),
            (2, "Bx"),
          )

          Schema.part_("b").a.get(partitionConn).sorted ==> List(
            ":b_Bc/bc1",
            ":b_Bx/bb1",
            ":b_Bx/bb2",
          )
        }
      }
    }


    test("Delete") {

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
            Part(1, "a", None, None, List()),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
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
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List()),
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
    }


    test("Restore") {
      ResetDbs.resetDbs()
    }
  }
}
