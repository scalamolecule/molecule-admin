package moleculeadmin.servertest.schema.namespace

import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object NamespaceCreate extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bx", "b_Bx", Some("Description of Bx"), None, List()),
            MetaNs(3, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List()),
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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bx", "b_Bx", None, None, List()),
            MetaNs(2, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(3, "Bc", "b_Bc", None, None, List(
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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(3, "Bx", "b_Bx", None, None, List()),
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
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(3, "Bx", "b_Bx", None, None, List()))),
          MetaPart(3, "c", None, None, List()),
        ))
      )
      // attr
      createAttribute(getMetaSchema("Partition"), "Partition", "b", "Bx", "bx1", 1, "Int") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(3, "Bx", "b_Bx", None, None, List(
              MetaAttr(1, "bx1", 1, "Int", Nil, None, Seq("indexed"), None, None, None, None, None, List()))))),
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

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}
