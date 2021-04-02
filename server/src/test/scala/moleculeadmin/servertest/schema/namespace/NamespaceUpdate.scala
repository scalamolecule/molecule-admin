package moleculeadmin.servertest.schema.namespace

import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object NamespaceUpdate extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", Some("description"), None, List(
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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", Some("other description"), None, List(
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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bx", "b_Bx", None, None, List(
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
        MetaPart(1, "a", None, None, List(
          MetaNs(1, "Aa", "a_Aa", None, None, List()))),
        MetaPart(2, "b", None, None, List(
          MetaNs(1, "Bb", "b_Bb", None, None, List(
            MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
            MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
          MetaNs(2, "Bc", "b_Bc", None, None, List(
            MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
            MetaAttr(2, "selfJoin", 1, "ref", Nil, Some("b_Bc"), Seq("indexed"), None, None, None, None, None, List())
          )))),
        MetaPart(3, "c", None, None, List()),
      ))

      updateNamespace(schema1, "Partition", "b", "Bc", "Bc1") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc1", "b_Bc1", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "selfJoin", 1, "ref", Nil, Some("b_Bc1"), Seq("indexed"), None, None, None, None, None, List())
            )))),
          MetaPart(3, "c", None, None, List()),
        ))
      )
    }


    test("All") {
      val ps = new PartitionSetup
      import ps._

      updateNamespace(partitionMetaSchema, "Partition", "b", "Bb", "Bx", Some("description"), 2) ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bx", "b_Bx", Some("description"), None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
          )),
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

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}
