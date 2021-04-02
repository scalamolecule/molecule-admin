package moleculeadmin.servertest.schema.attribute.update

import java.util
import ammonite.ops._
import datomic.Util
import db.admin.dsl.moleculeAdmin._
import db.core.dsl.coreTest.{Ns => Nsx}
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AttributeEnum extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Validation") {
      val ps = new PartitionSetup
      import ps._

      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enum", "enum",
        1, "Enum", Nil) ==> Left("Successfully rolled back from error: No enum values passed.")

      // Successfully rolled back (attribute `enum` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enum").pos.card.tpe.enums.get(moleculeAdminConn) ==> List(
        (12, 1, "String", Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8"))
      )

      // live schema
      Schema.attr("enum").card.tpe.enum.get(coretestConn).sortBy(_._4) ==> List(
        ("enum", "one", "ref", "enum0"),
        ("enum", "one", "ref", "enum1"),
        ("enum", "one", "ref", "enum2"),
        ("enum", "one", "ref", "enum3"),
        ("enum", "one", "ref", "enum4"),
        ("enum", "one", "ref", "enum5"),
        ("enum", "one", "ref", "enum6"),
        ("enum", "one", "ref", "enum7"),
        ("enum", "one", "ref", "enum8"),
        ("enum", "one", "ref", "enum9")
      )
    }


    test("Card one") {
      val ps = new PartitionSetup
      import ps._

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enum").enums.get(moleculeAdminConn).head.toList.sorted ==> List(
        "enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9"
      )

      // live - map cardinality is internally just cardinality `many`
      Schema.a_(":Ns/enum").enum.get(coretestConn).sorted ==> List(
        "enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9"
      )

      // Add 2 enum data values
      val List(e1, e2) = coretestConn.transact(
        Util.list(
          Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/enum", ":Ns.enum/enum1"),
          Util.list(":db/add", "#db/id[:db.part/user -1000002]", ":Ns/enum", ":Ns.enum/enum7")
        ).asInstanceOf[util.List[AnyRef]]
      ).eids

      // Live data
      coretestConn.q(s"[:find (distinct ?enum) :where [?e :Ns/enum ?ref] [?ref :db/ident ?ident] [(name ?ident) ?enum]]") ==>
        List(List(Set("enum1", "enum7")))

      // Can't remove enum value if it has been asserted ("enum7" is now obsolete)
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enum", "enum",
        1, "Enum", Seq(
          "enum0", "enum1", "enum2", // keep some existing enum values
          "man", "woman" // add new enum values
        )
      ) ==> Left(
        """Successfully rolled back from error: Couldn't remove obsolete enums having live values. Please remove values before removing enums from schema.
          |Conflicting obsolete enum values: enum7""".stripMargin)

      // Successfully rolled back (attribute `enum` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enum").pos.card.tpe.enums.get(moleculeAdminConn) ==> List(
        (12, 1, "String", Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8"))
      )

      // live schema
      Schema.attr("enum").card.tpe.enum.get(coretestConn).sortBy(_._4) ==> List(
        ("enum", "one", "ref", "enum0"),
        ("enum", "one", "ref", "enum1"),
        ("enum", "one", "ref", "enum2"),
        ("enum", "one", "ref", "enum3"),
        ("enum", "one", "ref", "enum4"),
        ("enum", "one", "ref", "enum5"),
        ("enum", "one", "ref", "enum6"),
        ("enum", "one", "ref", "enum7"),
        ("enum", "one", "ref", "enum8"),
        ("enum", "one", "ref", "enum9")
      )


      // Retract obsolete enum data value so that we can remove the enum schema value
      coretestConn.transact(
        Util.list(
          Util.list(s":db/retract", e2.asInstanceOf[AnyRef], ":Ns/enum", ":Ns.enum/enum7")
        ).asInstanceOf[util.List[AnyRef]]
      )

      // Now we can also remove the `enum7` schema enum value
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enum", "enum",
        1, "Enum", Seq(
          "enum0", "enum1", "enum2", // keep some existing enum values
          "man", "woman" // add new enum values
        )
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

              MetaAttr(12, "enum", 1, "String", Seq(
                "enum0", "enum1", "enum2",
                "man", "woman"
              ), None, Nil, None, None, None, None, None, List()),

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

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enum").enums.get(moleculeAdminConn) ==> List(Set("enum1", "man", "woman", "enum2", "enum0"))

      // live
      Schema.a_(":Ns/enum").enum.get(coretestConn) ==> List("woman", "enum2", "man", "enum1", "enum0")

      // New enums can now be asserted
      coretestConn.transact(
        Util.list(
          Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/enum", ":Ns.enum/woman")
        ).asInstanceOf[util.List[AnyRef]]
      ).eids

      // Value is saved
      coretestConn.q(s"[:find (distinct ?enum) :where [?e :Ns/enum ?ref] [?ref :db/ident ?ident] [(name ?ident) ?enum]]") ==>
        List(List(Set("enum1", "woman")))
    }


    test("Card many") {
      val ps = new PartitionSetup
      import ps._

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enums").enums.get(moleculeAdminConn).head.toList.sorted ==> List(
        "enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9"
      )

      // live - map cardinality is internally just cardinality `many`
      Schema.a_(":Ns/enums").enum.get(coretestConn).sorted ==> List(
        "enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9"
      )

      // Add 2 enum data values
      val List(e1, e2) = coretestConn.transact(
        Util.list(
          Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/enums", ":Ns.enums/enum1"),
          Util.list(":db/add", "#db/id[:db.part/user -1000002]", ":Ns/enums", ":Ns.enums/enum7")
        ).asInstanceOf[util.List[AnyRef]]
      ).eids

      // Live data
      coretestConn.q(s"[:find (distinct ?enum) :where [?e :Ns/enums ?ref] [?ref :db/ident ?ident] [(name ?ident) ?enum]]") ==>
        List(List(Set("enum1", "enum7")))

      // Can't remove enum value if it has been asserted ("enum7" is now obsolete)
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enums", "enums",
        2, "Enum", Seq(
          "enum0", "enum1", "enum2", // keep some existing enum values
          "man", "woman" // add new enum values
        )
      ) ==> Left(
        """Successfully rolled back from error: Couldn't remove obsolete enums having live values. Please remove values before removing enums from schema.
          |Conflicting obsolete enum values: enum7""".stripMargin)

      // Successfully rolled back (attribute `enum` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enums").pos.card.tpe.enums.get(moleculeAdminConn) ==> List(
        (27, 2, "String", Set("enum1", "enum4", "enum9", "enum3", "enum8", "enum6", "enum2", "enum7", "enum0", "enum5"))
      )

      // live schema
      Schema.attr("enums").card.tpe.enum.get(coretestConn).sortBy(_._4) ==> List(
        ("enums", "many", "ref", "enum0"),
        ("enums", "many", "ref", "enum1"),
        ("enums", "many", "ref", "enum2"),
        ("enums", "many", "ref", "enum3"),
        ("enums", "many", "ref", "enum4"),
        ("enums", "many", "ref", "enum5"),
        ("enums", "many", "ref", "enum6"),
        ("enums", "many", "ref", "enum7"),
        ("enums", "many", "ref", "enum8"),
        ("enums", "many", "ref", "enum9"),
      )


      // Retract obsolete enum data value so that we can remove the enum schema value
      coretestConn.transact(
        Util.list(
          Util.list(s":db/retract", e2.asInstanceOf[AnyRef], ":Ns/enums", ":Ns.enums/enum7")
        ).asInstanceOf[util.List[AnyRef]]
      )

      // Now we can also remove the `enum7` schema enum value
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enums", "enums",
        2, "Enum", Seq(
          "enum0", "enum1", "enum2", // keep some existing enum values
          "man", "woman" // add new enum values
        )
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

              MetaAttr(27, "enums", 2, "String", Seq(
                "enum0", "enum1", "enum2",
                "man", "woman"
              ), None, Nil, None, None, None, None, None, List()),

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

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enums").enums.get(moleculeAdminConn) ==> List(Set("enum1", "man", "woman", "enum2", "enum0"))

      // live
      Schema.a_(":Ns/enums").enum.get(coretestConn) ==> List("woman", "enum2", "man", "enum1", "enum0")

      // New enums can now be asserted
      coretestConn.transact(
        Util.list(
          Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/enums", ":Ns.enums/woman")
        ).asInstanceOf[util.List[AnyRef]]
      ).eids

      // Value is saved
      coretestConn.q(s"[:find (distinct ?enum) :where [?e :Ns/enums ?ref] [?ref :db/ident ?ident] [(name ?ident) ?enum]]") ==> List(
        List(Set("enum1", "woman")))

      Nsx.enums.get(coretestConn) ==> List(Set("enum1", "woman"))
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}