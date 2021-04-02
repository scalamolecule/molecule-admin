package moleculeadmin.servertest.schema.attribute.update

import java.util
import ammonite.ops._
import datomic.Util
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AttributeCardinality extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("One -> map not allowed") {
      val ps = new PartitionSetup
      import ps._

      // card one to map cardinality not allowed
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "int", "int", 3, "Int") ==> Left(
        "Successfully rolled back from error: " +
          "Couldn't change to map cardinality since keys are unknown. Please create a new map attribute and populate with keyed data."
      )

      // Successfully rolled back (attribute `int` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("int").pos.card.tpe.get(moleculeAdminConn) ==> List((2, 1, "Int"))

      // live schema
      Schema.attr("int").card.tpe.get(coretestConn) ==> List(("int", "one", "long"))
    }


    test("Many -> map not allowed") {
      val ps = new PartitionSetup
      import ps._

      // card many to map cardinality not allowed
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "ints", "ints", 3, "Int") ==> Left(
        "Successfully rolled back from error: " +
          "Couldn't change to map cardinality since keys are unknown. Please create a new map attribute and populate with keyed data."
      )

      // Successfully rolled back (attribute `ints` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("ints").pos.card.tpe.get(moleculeAdminConn) ==> List((17, 2, "Int"))

      // live schema
      Schema.attr("ints").card.tpe.get(coretestConn) ==> List(("ints", "many", "long"))
    }


    test("One -> many -> one") {
      val ps = new PartitionSetup
      import ps._

      val schema1 = updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb1", 2, "Int").getOrElse(MetaSchema(Nil))
      schema1 ==> MetaSchema(List(
        MetaPart(1, "a", None, None, List(
          MetaNs(1, "Aa", "a_Aa", None, None, List()))),
        MetaPart(2, "b", None, None, List(
          MetaNs(1, "Bb", "b_Bb", None, None, List(
            MetaAttr(1, "bb1", 2, "Int", Nil, None, Nil, None, None, None, None, None, List()),
            MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
          MetaNs(2, "Bc", "b_Bc", None, None, List(
            MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
        MetaPart(3, "c", None, None, List())
      ))

      val updatedDefFile =
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
          |      val bb1 = manyInt
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

      read ! partitionDefFilePath ==> updatedDefFile

      // meta
      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").card.get(moleculeAdminConn).sorted ==> List(2)

      // live
      Schema.a_(":b_Bb/bb1").card.get(partitionConn) ==> List("many")


      // Add set of 2 values to attr that is now cardinality-many
      val eid: Long = partitionConn.transact(
        Util.list(
          Util.list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 1.asInstanceOf[AnyRef]),
          Util.list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 2.asInstanceOf[AnyRef])
        ).asInstanceOf[util.List[AnyRef]]
      ).eid
      partitionConn.q(s"[:find (distinct ?v) :where [$eid :b_Bb/bb1 ?v]]") ==> List(List(Set(1, 2)))

      // Can't make change from card many to one if any entity has multiple values for the attribute
      updateAttribute(schema1, "Partition", "b", "Bb", "bb1", "bb1", 1, "Int") ==>
        Left("Successfully rolled back from error: " +
          "Couldn't change attribute to cardinality 1 since some entities have multiple values. Please reduce all values to one value before changing cardinality.")

      // Successfully rolled back (attribute `bb1` still cardinality 2)

      // client
      getMetaSchema("Partition") ==> schema1

      // def file
      read ! partitionDefFilePath ==> updatedDefFile

      // meta
      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").pos.card.tpe.get(moleculeAdminConn) ==> List((1, 2, "Int"))

      // live schema
      Schema.attr("bb1").card.tpe.get(partitionConn) ==> List(("bb1", "many", "long"))


      // Make all entities (1 here) have maximum 1 value for the attribute
      partitionConn.transact(
        Util.list(
          Util.list(s":db/retract", eid.asInstanceOf[AnyRef], ":b_Bb/bb1", 2.asInstanceOf[AnyRef])
        ).asInstanceOf[util.List[AnyRef]]
      )
      partitionConn.q(s"[:find (distinct ?v) :where [$eid :b_Bb/bb1 ?v]]") ==> List(List(Set(1)))

      // Now we can change cardinality from many back to one
      updateAttribute(schema1, "Partition", "b", "Bb", "bb1", "bb1", 1, "Int") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List())
        ))
      )

      // Def file back to original
      read ! partitionDefFilePath ==> partitionDefFile

      // meta
      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").card.get(moleculeAdminConn).sorted ==> List(1)

      // live
      Schema.a_(":b_Bb/bb1").card.get(partitionConn) ==> List("one")
    }


    test("Map -> one (String)") {
      val ps = new PartitionSetup
      import ps._

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").card.get(moleculeAdminConn).sorted ==> List(3)

      // live - map cardinality is internally just cardinality `many`
      Schema.a_(":Ns/strMap").card.get(coretestConn) ==> List("many")

      // Create 2 entities with 2/1 values
      val List(e1, e2) = coretestConn.transact(
        Util.list(
          Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/strMap", "key1@hello"),
          Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/strMap", "key2@world"),
          Util.list(":db/add", "#db/id[:db.part/user -1000002]", ":Ns/strMap", "key3@mister")
        ).asInstanceOf[util.List[AnyRef]]
      ).eids

      coretestConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
        List(e1, Set("key1@hello", "key2@world")),
        List(e2, Set("key3@mister"))
      )

      // Can't make change from card map to one if any entity has multiple values for the attribute
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "strMap", "strMap",
        1, "String") ==> Left("Successfully rolled back from error: " +
        "Couldn't change attribute to cardinality 1 since some entities have multiple values. Please reduce all values to one value before changing cardinality.")

      // Successfully rolled back (attribute `intMap` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").pos.card.tpe.get(moleculeAdminConn) ==> List((31, 3, "String"))

      // live schema
      Schema.attr("strMap").card.tpe.get(coretestConn) ==> List(("strMap", "many", "string"))

      // Data intact
      coretestConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
        List(e1, Set("key1@hello", "key2@world")),
        List(e2, Set("key3@mister"))
      )


      // Make all entities (1 here) have maximum 1 value for the attribute
      coretestConn.transact(
        Util.list(
          Util.list(s":db/retract", e1.asInstanceOf[AnyRef], ":Ns/strMap", "key2@world")
        ).asInstanceOf[util.List[AnyRef]]
      )

      coretestConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
        List(e1, Set("key1@hello")),
        List(e2, Set("key3@mister"))
      )

      // Now we can change cardinality from map to one
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "strMap", "strMap",
        1, "String", Nil, None, Seq("fulltext")
      ) ==> Right(
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
              MetaAttr(14, "ref1", 1, "ref", Nil, Some("Ref1"), Nil, None, None, None, None, None, List()),
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

              MetaAttr(31, "strMap", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, List()),

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

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").card.get(moleculeAdminConn).sorted ==> List(1)

      // live
      Schema.a_(":Ns/strMap").card.get(coretestConn) ==> List("one")

      // Keys are removed from String values
      coretestConn.q(s"[:find ?e ?v :where [?e :Ns/strMap ?v]]") ==> List(
        List(e1, "hello"),
        List(e2, "mister")
      )
    }


    test("Map -> one (Int/other)") {
      val ps = new PartitionSetup
      import ps._

      // Now we can change cardinality from map to one
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "intMap", "intMap",
        1, "Int") ==> Left("Successfully rolled back from error: " +
        "Changing map cardinality when not of type String not supported yet (would require creating a new attribute of the target type)."
      )

      // Successfully rolled back (attribute `intMap` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("intMap").pos.card.tpe.get(moleculeAdminConn) ==> List((32, 3, "Int"))

      // live schema
      Schema.attr("intMap").card.tpe.get(coretestConn) ==> List(("intMap", "many", "string"))
    }


    test("Map -> many (String)") {
      val ps = new PartitionSetup
      import ps._

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").card.get(moleculeAdminConn).sorted ==> List(3)

      // live - map cardinality is internally just cardinality `many`
      Schema.a_(":Ns/strMap").card.get(coretestConn) ==> List("many")

      // Create 2 entities with 2/1 values
      val List(e1, e2) = coretestConn.transact(
        Util.list(
          Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/strMap", "key1@hello"),
          Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/strMap", "key2@world"),
          Util.list(":db/add", "#db/id[:db.part/user -1000002]", ":Ns/strMap", "key2@mister")
        ).asInstanceOf[util.List[AnyRef]]
      ).eids

      coretestConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
        List(e1, Set("key1@hello", "key2@world")),
        List(e2, Set("key2@mister"))
      )

      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "strMap", "strMap",
        2, "String", Nil, None, Seq("fulltext")
      ) ==> Right(
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
              MetaAttr(14, "ref1", 1, "ref", Nil, Some("Ref1"), Nil, None, None, None, None, None, List()),
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

              MetaAttr(31, "strMap", 2, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, List()),

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

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").card.get(moleculeAdminConn).sorted ==> List(2)

      // live
      Schema.a_(":Ns/strMap").card.get(coretestConn) ==> List("many")

      // Keys are removed from String values
      coretestConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
        List(e1, Set("hello", "world")),
        List(e2, Set("mister"))
      )
    }


    test("Map -> many (Int)") {
      val ps = new PartitionSetup
      import ps._

      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "intMap", "intMap",
        2, "Int") ==> Left("Successfully rolled back from error: " +
        "Changing map cardinality when not of type String not supported yet (would require creating a new attribute of the target type)."
      )

      // Successfully rolled back (attribute `intMap` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("intMap").pos.card.tpe.get(moleculeAdminConn) ==> List((32, 3, "Int"))

      // live schema
      Schema.attr("intMap").card.tpe.get(coretestConn) ==> List(("intMap", "many", "string"))
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}