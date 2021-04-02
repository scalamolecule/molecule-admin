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


object AttributeCreate extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Basic validation") {
      val ps = new PartitionSetup
      import ps._

      createAttribute(partitionMetaSchema, "Partition", "a", "Aa", "", 1, "Int") ==> Left(
        "Empty attribute name."
      )

      createAttribute(partitionMetaSchema, "Partition", "a", "Aa", "Aa1", 1, "Int") ==> Left(
        "Attribute name should start with lowercase letter and match `[a-z][a-zA-Z0-9]*`."
      )

      createAttribute(partitionMetaSchema, "Partition", "a", "Aa", "a-1", 1, "Int") ==> Left(
        "Attribute name should match `[a-z][a-zA-Z0-9]*`."
      )

      createAttribute(partitionMetaSchema, "Partition", "a", "Aa", "op", 1, "Int") ==> Left(
        """Reserved attribute names:
          |a
          |apply
          |assert
          |contains
          |e
          |insert
          |k
          |not
          |op
          |replace
          |retract
          |save
          |self
          |t
          |tx
          |txInstant
          |update
          |v""".stripMargin
      )

      createAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", 1, "Int") ==> Left(
        "Attribute `bb1` already exists in namespace `Bb` in partition `b` in database `Partition`. Please choose another attribute name."
      )

      createAttribute(partitionMetaSchema, "qqq", "a", "Bc", "aa1", 1, "Int") ==> Left(
        "Couldn't find database `qqq`."
      )

      createAttribute(partitionMetaSchema, "Partition", "z", "Bc", "aa1", 1, "Int") ==> Left(
        "Couldn't find partition `z` in database `Partition`."
      )

      createAttribute(partitionMetaSchema, "Partition", "b", "Bz", "aa1", 1, "Int") ==> Left(
        "Couldn't find namespace `Bz` in partition `b` in database `Partition`."
      )
    }


    test("Int, card 1, first pos, descr") {
      val ps = new PartitionSetup
      import ps._

      // Client schema
      createAttribute(partitionMetaSchema, "Partition", "a", "Aa",
        "number", 1, "Int", Nil, None, Nil, Some("number description")).getOrElse(MetaSchema(Nil)).parts.head ==>
        MetaPart(1, "a", None, None, Seq(
          MetaNs(1, "Aa", "a_Aa", None, None, Seq(
            MetaAttr(1, "number", 1, "Int", Nil, None, Seq("indexed"), Some("number description"), None, None, None, None, List())))))

      // def file has namespace prepared, here positioned first
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
          |      val number = oneInt.doc("number description")
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

      // meta schema (saving meta partition for possible ns/attr additions)
      meta_Db.name_("Partition").Partitions.name_("a").Namespaces.name_("Aa").Attrs.name.get(moleculeAdminConn).sorted ==> List("number")

      // live namespaces having defined attributes
      Schema.ns_("Aa").attr.get(partitionConn).sorted ==> List("number")

      // Data - we can use the new attribute
      partitionConn.transact(list(list(":db/add", "#db/id[:a -1000001]", ":a_Aa/number", 42.asInstanceOf[Object])).asInstanceOf[util.List[AnyRef]])
      partitionConn.q("[:find ?value :where [_ :a_Aa/number ?value]]") ==> List(List(42))
    }


    test("Int, card 1, first pos, option, descr") {
      val ps = new PartitionSetup
      import ps._

      // Client schema
      createAttribute(partitionMetaSchema, "Partition", "a", "Aa",
        "number", 1, "Int", Nil, None, Seq("noHistory"), Some("number description")).getOrElse(MetaSchema(Nil)).parts.head ==>
        MetaPart(1, "a", None, None, Seq(
          MetaNs(1, "Aa", "a_Aa", None, None, Seq(
            MetaAttr(1, "number", 1, "Int", Nil, None, Seq("indexed", "noHistory"), Some("number description"), None, None, None, None, List())))))

      // def file has namespace prepared, here positioned first
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
          |      val number = oneInt.noHistory.doc("number description")
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

      // meta schema (saving meta partition for possible ns/attr additions)
      meta_Db.name_("Partition").Partitions.name_("a").Namespaces.name_("Aa")
        .Attrs.name.options.get(moleculeAdminConn) ==> List(("number", Set("indexed", "noHistory")))

      // live namespaces having defined attributes
      Schema.ns_("Aa").attr.noHistory.get(partitionConn) ==> List(("number", true))

      // Data - we can use the new attribute
      partitionConn.transact(list(list(":db/add", "#db/id[:a -1000001]", ":a_Aa/number", 42.asInstanceOf[Object])).asInstanceOf[util.List[AnyRef]])
      partitionConn.q("[:find ?value :where [_ :a_Aa/number ?value]]") ==> List(List(42))
    }


    test("Enum, card 2, middle pos, options, descr") {
      val ps = new PartitionSetup
      import ps._

      createAttribute(partitionMetaSchema, "Partition", "b", "Bb", "text",
        2, "String", Seq("enum1", "enum2"), None, Seq("fulltext", "noHistory", "uniqueValue"), Some("descr"), 2).getOrElse(MetaSchema(Nil)).parts(1) ==>
        MetaPart(2, "b", None, None, List(
          MetaNs(1, "Bb", "b_Bb", None, None, List(
            MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
            MetaAttr(2, "text", 2, "String", Seq("enum1", "enum2"), None, Seq("indexed", "fulltext", "noHistory", "uniqueValue"), Some("descr"), None, None, None, None, List()),
            MetaAttr(3, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
          MetaNs(2, "Bc", "b_Bc", None, None, List(
            MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List())))))

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
          |      val bb1  = oneInt
          |      val text = manyEnum("enum1", "enum2").fulltext.noHistory.uniqueValue.doc("descr")
          |      val bb2  = oneInt
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

      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name.get(moleculeAdminConn).sorted ==> List("bb1", "bb2", "text")

      Schema.ns_("Bb").attr.get(partitionConn).sorted ==> List("bb1", "bb2", "text")

      // Data - we can use the new enum attribute
      partitionConn.transact(list(list(":db/add", "#db/id[:a -1000001]", ":b_Bb/text", ":b_Bb.text/enum1")).asInstanceOf[util.List[AnyRef]])
      partitionConn.q(
        """[:find  (distinct ?b2)
          | :where [?a :b_Bb/text ?b]
          |        [?b :db/ident ?b1]
          |        [(name ?b1) ?b2]]""".stripMargin) ==> List(List(Set("enum1")))
    }


    test("Ref (card 1)") {
      val ps = new PartitionSetup
      import ps._

      createAttribute(partitionMetaSchema, "Partition", "b", "Bc",
        "ref", 1, "ref", Nil, Some("b_Bb")).getOrElse(MetaSchema(Nil)).parts(1) ==>
        MetaPart(2, "b", None, None, List(
          MetaNs(1, "Bb", "b_Bb", None, None, List(
            MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
            MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
          MetaNs(2, "Bc", "b_Bc", None, None, List(
            MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
            MetaAttr(2, "ref", 1, "ref", Nil, Some("b_Bb"), Seq("indexed"), None, None, None, None, None, List())))))

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
          |      val ref = one[Bb]
          |    }
          |  }
          |
          |  object c {
          |
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bc").Attrs.name.get(moleculeAdminConn).sorted ==> List("bc1", "ref")

      Schema.ns_("Bc").attr.get(partitionConn).sorted ==> List("bc1", "ref")

      // Data - we can use the new reference
      partitionConn.transact(
        list(
          list(":db/add", "#db/id[:b -1000001]", ":b_Bc/bc1", 42.asInstanceOf[Object]),
          list(":db/add", "#db/id[:b -1000001]", ":b_Bc/ref", "#db/id[:b -1000002]"),
          list(":db/add", "#db/id[:b -1000002]", ":b_Bb/bb1", 43.asInstanceOf[Object]),
        ).asInstanceOf[util.List[AnyRef]]
      )
      partitionConn.q(
        """[:find  ?b ?d
          | :where [?a :b_Bc/bc1 ?b]
          |        [?a :b_Bc/ref ?c]
          |        [?c :b_Bb/bb1 ?d]]""".stripMargin) ==> List(List(42, 43))
    }


    test("Ref (card 2) to other partition") {
      val ps = new PartitionSetup
      import ps._

      createAttribute(partitionMetaSchema, "Partition", "a", "Aa",
        "ref", 2, "ref", Nil, Some("b_Bb")) ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List(
              MetaAttr(1, "ref", 2, "ref", Nil, Some("b_Bb"), Seq("indexed"), None, None, None, None, None, List()),
            )))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List())
        )
        ))

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
          |      val ref = many[b.Bb]
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

      meta_Db.name_("Partition").Partitions.name_("a").Namespaces.name_("Aa").Attrs.name.get(moleculeAdminConn).sorted ==> List("ref")

      Schema.ns_("Aa").attr.get(partitionConn).sorted ==> List("ref")

      // Data - we can use the new reference between two partitions
      partitionConn.transact(
        list(
          list(":db/add", "#db/id[:a -1000001]", ":a_Aa/ref", "#db/id[:b -1000002]"),
          list(":db/add", "#db/id[:b -1000002]", ":b_Bb/bb1", 43.asInstanceOf[Object]),
        ).asInstanceOf[util.List[AnyRef]]
      )
      partitionConn.q(
        """[:find  ?c
          | :where [?a :a_Aa/ref ?b]
          |        [?b :b_Bb/bb1 ?c]]""".stripMargin) ==> List(List(43))
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}