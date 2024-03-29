package moleculeadmin.servertest.schema.attribute.update

import java.util
import ammonite.ops._
import datomic.Util
import datomic.Util.list
import db.admin.dsl.moleculeAdmin._
import db.core.dsl.coreTest.{Ns => Nsx}
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AttributeRefNs extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Validation") {
      val ps = new PartitionSetup
      import ps._

      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "ref1", "ref1",
        1, "ref", Nil, Some("xx")) ==> Left(
        "Successfully rolled back from error: Couldn't find ref namespace `xx` in client schema."
      )

      // Successfully rolled back (attribute `ref1` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("ref1").pos.card.tpe.get(moleculeAdminConn) ==> List((14, 1, "ref"))

      // live schema
      Schema.attr("ref1").card.tpe.get(coretestConn) ==> List(("ref1", "one", "ref"))
    }


    test("Within partition") {
      val ps = new Partition1Setup
      import ps._

      // Add `refAttr` pointing to `Bc`
      val schema1: MetaSchema = createAttribute(partition1MetaSchema, "Partition1", "b", "Bb", "refAttr",
        1, "ref", Nil, Some("b_Bc")).getOrElse(MetaSchema(Nil))

      // Let `refAttr` point to Bd instead
      updateAttribute(schema1, "Partition1", "b", "Bb", "refAttr", "refAttr",
        1, "ref", Nil, Some("b_Bd")) ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(3, "bb3", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(4, "bb4", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(5, "refAttr", 1, "ref", Nil, Some("b_Bd"), Seq("indexed"), None, None, None, None, None, List())
            )),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(3, "Bd", "b_Bd", None, None, List(
              MetaAttr(1, "bd1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List())))
          )),
          MetaPart(3, "c", None, None, List())
        ))
      )

      read ! partition1DefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object Partition1Definition {
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
          |      val bb1     = oneInt
          |      val bb2     = oneInt
          |      val bb3     = oneInt
          |      val bb4     = oneInt
          |      val refAttr = one[Bd]
          |    }
          |
          |    trait Bc {
          |      val bc1 = oneInt
          |    }
          |
          |    trait Bd {
          |      val bd1 = oneInt
          |    }
          |  }
          |
          |  object c {
          |
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition1").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name.get(moleculeAdminConn).sorted ==> List("bb1", "bb2", "bb3", "bb4", "refAttr")

      Schema.ns_("Bb").attr.get(partition1Conn).sorted ==> List("bb1", "bb2", "bb3", "bb4", "refAttr")

      // Data - we can use the new reference
      partition1Conn.transact(
        list(
          list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 42.asInstanceOf[Object]),
          list(":db/add", "#db/id[:b -1000001]", ":b_Bb/refAttr", "#db/id[:b -1000002]"),
          list(":db/add", "#db/id[:b -1000002]", ":b_Bc/bc1", 43.asInstanceOf[Object])
        ).asInstanceOf[util.List[AnyRef]]
      )
      partition1Conn.q(
        """[:find  ?b ?d
          | :where [?a :b_Bb/bb1 ?b]
          |        [?a :b_Bb/refAttr ?c]
          |        [?c :b_Bc/bc1 ?d]]""".stripMargin) ==> List(List(42, 43))
    }


    test("To other partition") {
      val ps = new Partition1Setup
      import ps._

      // Add `refAttr` pointing to `Bc`
      val schema1: MetaSchema = createAttribute(partition1MetaSchema, "Partition1", "b", "Bb", "refAttr",
        1, "ref", Nil, Some("b_Bc")).getOrElse(MetaSchema(Nil))

      // Add attribute to namespace `Aa`
      val schema2: MetaSchema = createAttribute(schema1, "Partition1", "a", "Aa", "aa1",
        1, "Int").getOrElse(MetaSchema(Nil))

      // Let `refAttr` point to Bd instead
      updateAttribute(schema2, "Partition1", "b", "Bb", "refAttr", "refAttr",
        1, "ref", Nil, Some("a_Aa")) ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List(
              MetaAttr(1, "aa1", 1, "Int", Nil, None, Seq("indexed"), None, None, None, None, None, List()
            ))))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(3, "bb3", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(4, "bb4", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(5, "refAttr", 1, "ref", Nil, Some("a_Aa"), Seq("indexed"), None, None, None, None, None, List())
            )),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(3, "Bd", "b_Bd", None, None, List(
              MetaAttr(1, "bd1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List())))
          )),
          MetaPart(3, "c", None, None, List())
        ))
      )

      read ! partition1DefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object Partition1Definition {
          |
          |  object a {
          |
          |    trait Aa {
          |      val aa1 = oneInt
          |    }
          |  }
          |
          |  object b {
          |
          |    trait Bb {
          |      val bb1     = oneInt
          |      val bb2     = oneInt
          |      val bb3     = oneInt
          |      val bb4     = oneInt
          |      val refAttr = one[a.Aa]
          |    }
          |
          |    trait Bc {
          |      val bc1 = oneInt
          |    }
          |
          |    trait Bd {
          |      val bd1 = oneInt
          |    }
          |  }
          |
          |  object c {
          |
          |  }
          |}
          |""".stripMargin

      meta_Db.name_("Partition1").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name.get(moleculeAdminConn).sorted ==> List("bb1", "bb2", "bb3", "bb4", "refAttr")

      Schema.ns_("Bb").attr.get(partition1Conn).sorted ==> List("bb1", "bb2", "bb3", "bb4", "refAttr")

      // Data - we can use the new reference
      partition1Conn.transact(
        list(
          list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 42.asInstanceOf[Object]),
          list(":db/add", "#db/id[:b -1000001]", ":b_Bb/refAttr", "#db/id[:b -1000002]"),
          list(":db/add", "#db/id[:b -1000002]", ":a_Aa/aa1", 43.asInstanceOf[Object])
        ).asInstanceOf[util.List[AnyRef]]
      )
      partition1Conn.q(
        """[:find  ?b ?d
          | :where [?a :b_Bb/bb1 ?b]
          |        [?a :b_Bb/refAttr ?c]
          |        [?c :a_Aa/aa1 ?d]]""".stripMargin) ==> List(List(42, 43))
    }


    test("No partitions") {
      val ps = new PartitionSetup
      import ps._

      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "ref1", "ref1",
        1, "ref", Nil, Some("Ref2")) ==> Right(
        MetaSchema(List(
          MetaPart(1, "db.part/user", None, None, List(
            MetaNs(1, "Ns", "Ns", None, None, List(
              MetaAttr(1, "str", 1, "String", Nil, None, Seq("fulltext"), Some("Card one String attribute"), None, None, None, None, List()),
              MetaAttr(2, "int", 1, "Int", Nil, None, Nil, Some("Card one Int attribute"), None, None, None, None, List()),
              MetaAttr(3, "long", 1, "Long", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(4, "float", 1, "Float", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(5, "double", 1, "Double", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(6, "bool", 1, "Boolean", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(7, "bigInt", 1, "BigInt", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(8, "bigDec", 1, "BigDecimal", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(9, "date", 1, "Date", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(10, "uuid", 1, "UUID", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(11, "uri", 1, "URI", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(12, "enum", 1, "String", Seq("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8"), None, Nil, None, None, None, None, None, List()),
              MetaAttr(13, "parent", 1, "ref", Nil, Some("Ns"), Nil, None, None, None, None, None, List()),

              // Ref namespace changed from `ref1` to `ref2`
              MetaAttr(14, "ref1", 1, "ref", Nil, Some("Ref2"), Nil, None, None, None, None, None, List()),

              MetaAttr(15, "refSub1", 1, "ref", Nil, Some("Ref1"), Seq("isComponent"), None, None, None, None, None, List()),
              MetaAttr(16, "strs", 2, "String", Nil, None, Seq("fulltext"), None, Some(""), None, None, None, List()),
              MetaAttr(17, "ints", 2, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(18, "longs", 2, "Long", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(19, "floats", 2, "Float", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(20, "doubles", 2, "Double", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(21, "bools", 2, "Boolean", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(22, "bigInts", 2, "BigInt", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(23, "bigDecs", 2, "BigDecimal", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(24, "dates", 2, "Date", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(25, "uuids", 2, "UUID", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(26, "uris", 2, "URI", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(27, "enums", 2, "String", Seq("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8"), None, Nil, None, None, None, None, None, List()),
              MetaAttr(28, "parents", 2, "ref", Nil, Some("Ns"), Nil, None, None, None, None, None, List()),
              MetaAttr(29, "refs1", 2, "ref", Nil, Some("Ref1"), Nil, None, None, None, None, None, List()),
              MetaAttr(30, "refsSub1", 2, "ref", Nil, Some("Ref1"), Seq("isComponent"), None, None, None, None, None, List()),
              MetaAttr(31, "strMap", 3, "String", Nil, None, Seq("fulltext"), None, Some(""), None, None, None, List()),
              MetaAttr(32, "intMap", 3, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(33, "longMap", 3, "Long", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(34, "floatMap", 3, "Float", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(35, "doubleMap", 3, "Double", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(36, "boolMap", 3, "Boolean", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(37, "bigIntMap", 3, "BigInt", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(38, "bigDecMap", 3, "BigDecimal", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(39, "dateMap", 3, "Date", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(40, "uuidMap", 3, "UUID", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(41, "uriMap", 3, "URI", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Ref1", "Ref1", None, None, List(
              MetaAttr(1, "str1", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, List()),
              MetaAttr(2, "int1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(3, "enum1", 1, "String", Seq("enum12", "enum10", "enum11"), None, Nil, None, None, None, None, None, List()),
              MetaAttr(4, "ref2", 1, "ref", Nil, Some("Ref2"), Nil, None, None, None, None, None, List()),
              MetaAttr(5, "refSub2", 1, "ref", Nil, Some("Ref2"), Seq("isComponent"), None, None, None, None, None, List()),
              MetaAttr(6, "strs1", 2, "String", Nil, None, Nil, None, Some(""), None, None, None, List()),
              MetaAttr(7, "ints1", 2, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(8, "refs2", 2, "ref", Nil, Some("Ref2"), Nil, None, None, None, None, None, List()),
              MetaAttr(9, "refsSub2", 2, "ref", Nil, Some("Ref2"), Seq("isComponent"), None, None, None, None, None, List()))),
            MetaNs(3, "Ref2", "Ref2", None, None, List(
              MetaAttr(1, "str2", 1, "String", Nil, None, Seq("uniqueIdentity"), None, None, None, None, None, List()),
              MetaAttr(2, "int2", 1, "Int", Nil, None, Seq("uniqueValue"), None, None, None, None, None, List()),
              MetaAttr(3, "enum2", 1, "String", Seq("enum22", "enum20", "enum21"), None, Nil, None, None, None, None, None, List()),
              MetaAttr(4, "strs2", 2, "String", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(5, "ints2", 2, "Int", Nil, None, Seq("noHistory"), None, None, None, None, None, List()))))
        )))
      )
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition", "Partition1"))
    }
  }
}