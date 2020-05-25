package moleculeadmin.servertest.schema.withPartitions

import ammonite.ops._
import molecule.facade.Conn
import moleculeadmin.server.Schema
import moleculeadmin.servertest.ResetDbs
import moleculeadmin.shared.ast.schema._


trait Settings {

  val pwd = home / "molecule" / "molecule-admin" / "molecule-admin"

  val partitionDefFile =
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

  val partitionDefFilePath = pwd / "server" / "src" / "main" / "scala" / "db" / "migration" / "schema" / "PartitionDefinition.scala"

  val partitionMetaSchema = MetaSchema(List(
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


  // Reset def file
  val partition1DefFile =
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
      |      val bb1 = oneInt
      |      val bb2 = oneInt
      |      val bb3 = oneInt
      |      val bb4 = oneInt
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

  val partition1DefFilePath = pwd / "server" / "src" / "main" / "scala" / "db" / "migration" / "schema" / "Partition1Definition.scala"

  val partition1MetaSchema = MetaSchema(List(
    Part(1, "a", None, None, List(
      Ns(1, "Aa", "a_Aa", None, None, List()))),
    Part(2, "b", None, None, List(
      Ns(1, "Bb", "b_Bb", None, None, List(
        Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
        Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
        Attr(3, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
        Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List()),
      )),
      Ns(2, "Bc", "b_Bc", None, None, List(
        Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
      Ns(3, "Bd", "b_Bd", None, None, List(
        Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
    )),
    Part(3, "c", None, None, List()),
  ))



  // Reset def file
  val partition2DefFile =
    """package db.migration.schema
      |
      |import molecule.schema.definition._
      |
      |@InOut(0, 5)
      |object Partition2Definition {
      |
      |  object a {
      |
      |    trait Aa {
      |      val aa1 = oneInt
      |      val abb = one[b.Bb]
      |      val abc = one[b.Bc]
      |      val abd = one[b.Bd]
      |    }
      |  }
      |
      |  object b {
      |
      |    trait Bb {
      |      val bb1 = oneInt
      |      val bb2 = oneInt
      |      val bb3 = oneInt
      |      val bb4 = oneInt
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
      |    trait Cc {
      |      val cc1 = oneInt
      |    }
      |  }
      |}
      |""".stripMargin

  val partition2DefFilePath = pwd / "server" / "src" / "main" / "scala" / "db" / "migration" / "schema" / "Partition2Definition.scala"


  val coreDefFile =
    """package db.core.schema
      |import molecule.schema.definition._
      |
      |@InOut(3, 22)
      |object CoreTestDefinition {
      |
      |  trait Ns {
      |    val str     = oneString.fulltext.doc("Card one String attribute")
      |    val int     = oneInt.doc("Card one Int attribute")
      |    val long    = oneLong
      |    val float   = oneFloat
      |    val double  = oneDouble
      |    val bool    = oneBoolean
      |    val bigInt  = oneBigInt
      |    val bigDec  = oneBigDecimal
      |    val date    = oneDate
      |    val uuid    = oneUUID
      |    val uri     = oneURI
      |    val enum    = oneEnum("enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9")
      |    val parent  = one[Ns]
      |    val ref1    = one[Ref1]
      |    val refSub1 = one[Ref1].isComponent
      |
      |    val strs     = manyString.fulltext
      |    val ints     = manyInt
      |    val longs    = manyLong
      |    val floats   = manyFloat
      |    val doubles  = manyDouble
      |    val bools    = manyBoolean
      |    val bigInts  = manyBigInt
      |    val bigDecs  = manyBigDecimal
      |    val dates    = manyDate
      |    val uuids    = manyUUID
      |    val uris     = manyURI
      |    val enums    = manyEnum("enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9")
      |    val parents  = many[Ns]
      |    val refs1    = many[Ref1]
      |    val refsSub1 = many[Ref1].isComponent
      |
      |    val strMap    = mapString.fulltext
      |    val intMap    = mapInt
      |    val longMap   = mapLong
      |    val floatMap  = mapFloat
      |    val doubleMap = mapDouble
      |    val boolMap   = mapBoolean
      |    val bigIntMap = mapBigInt
      |    val bigDecMap = mapBigDecimal
      |    val dateMap   = mapDate
      |    val uuidMap   = mapUUID
      |    val uriMap    = mapURI
      |  }
      |
      |  trait Ref1 {
      |    val str1    = oneString.fulltext
      |    val int1    = oneInt
      |    val enum1   = oneEnum("enum10", "enum11", "enum12")
      |    val ref2    = one[Ref2]
      |    val refSub2 = one[Ref2].isComponent
      |
      |    val strs1    = manyString
      |    val ints1    = manyInt
      |    val refs2    = many[Ref2]
      |    val refsSub2 = many[Ref2].isComponent
      |  }
      |
      |  trait Ref2 {
      |    val str2  = oneString.uniqueIdentity
      |    val int2  = oneInt.uniqueValue
      |    val enum2 = oneEnum("enum20", "enum21", "enum22")
      |    val strs2 = manyString
      |    val ints2 = manyInt.noHistory
      |  }
      |}
      |""".stripMargin

  val coreDefFilePath = pwd / "server" / "src" / "main" / "scala" / "db" / "core" / "schema" / "CoreTestDefinition.scala"

  val coreMetaSchema = MetaSchema(Seq(
    Part(1, "db.part/user", None, None, List(
      Ns(1, "Ns", "Ns", None, None, List(
        Attr(1, "str", 1, "String", None, None, Some(Set("fulltext")), Some("Card one String attribute"), None, None, None, None, List()),
        Attr(2, "int", 1, "Int", None, None, None, Some("Card one Int attribute"), None, None, None, None, List()),
        Attr(3, "long", 1, "Long", None, None, None, None, None, None, None, None, List()),
        Attr(4, "float", 1, "Float", None, None, None, None, None, None, None, None, List()),
        Attr(5, "double", 1, "Double", None, None, None, None, None, None, None, None, List()),
        Attr(6, "bool", 1, "Boolean", None, None, None, None, None, None, None, None, List()),
        Attr(7, "bigInt", 1, "BigInt", None, None, None, None, None, None, None, None, List()),
        Attr(8, "bigDec", 1, "BigDecimal", None, None, None, None, None, None, None, None, List()),
        Attr(9, "date", 1, "Date", None, None, None, None, None, None, None, None, List()),
        Attr(10, "uuid", 1, "UUID", None, None, None, None, None, None, None, None, List()),
        Attr(11, "uri", 1, "URI", None, None, None, None, None, None, None, None, List()),
        Attr(12, "enum", 1, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
        Attr(13, "parent", 1, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
        Attr(14, "ref1", 1, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
        Attr(15, "refSub1", 1, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
        Attr(16, "strs", 2, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
        Attr(17, "ints", 2, "Int", None, None, None, None, None, None, None, None, List()),
        Attr(18, "longs", 2, "Long", None, None, None, None, None, None, None, None, List()),
        Attr(19, "floats", 2, "Float", None, None, None, None, None, None, None, None, List()),
        Attr(20, "doubles", 2, "Double", None, None, None, None, None, None, None, None, List()),
        Attr(21, "bools", 2, "Boolean", None, None, None, None, None, None, None, None, List()),
        Attr(22, "bigInts", 2, "BigInt", None, None, None, None, None, None, None, None, List()),
        Attr(23, "bigDecs", 2, "BigDecimal", None, None, None, None, None, None, None, None, List()),
        Attr(24, "dates", 2, "Date", None, None, None, None, None, None, None, None, List()),
        Attr(25, "uuids", 2, "UUID", None, None, None, None, None, None, None, None, List()),
        Attr(26, "uris", 2, "URI", None, None, None, None, None, None, None, None, List()),
        Attr(27, "enums", 2, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
        Attr(28, "parents", 2, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
        Attr(29, "refs1", 2, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
        Attr(30, "refsSub1", 2, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
        Attr(31, "strMap", 3, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
        Attr(32, "intMap", 3, "Int", None, None, None, None, None, None, None, None, List()),
        Attr(33, "longMap", 3, "Long", None, None, None, None, None, None, None, None, List()),
        Attr(34, "floatMap", 3, "Float", None, None, None, None, None, None, None, None, List()),
        Attr(35, "doubleMap", 3, "Double", None, None, None, None, None, None, None, None, List()),
        Attr(36, "boolMap", 3, "Boolean", None, None, None, None, None, None, None, None, List()),
        Attr(37, "bigIntMap", 3, "BigInt", None, None, None, None, None, None, None, None, List()),
        Attr(38, "bigDecMap", 3, "BigDecimal", None, None, None, None, None, None, None, None, List()),
        Attr(39, "dateMap", 3, "Date", None, None, None, None, None, None, None, None, List()),
        Attr(40, "uuidMap", 3, "UUID", None, None, None, None, None, None, None, None, List()),
        Attr(41, "uriMap", 3, "URI", None, None, None, None, None, None, None, None, List()))),
      Ns(2, "Ref1", "Ref1", None, None, List(
        Attr(1, "str1", 1, "String", None, None, None, None, None, None, None, None, List()),
        Attr(2, "int1", 1, "Int", None, None, None, None, None, None, None, None, List()),
        Attr(3, "enum1", 1, "String", Some(Set("enum12", "enum10", "enum11")), None, None, None, None, None, None, None, List()),
        Attr(4, "ref2", 1, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
        Attr(5, "refSub2", 1, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()),
        Attr(6, "strs1", 2, "String", None, None, None, None, Some(""), None, None, None, List()),
        Attr(7, "ints1", 2, "Int", None, None, None, None, None, None, None, None, List()),
        Attr(8, "refs2", 2, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
        Attr(9, "refsSub2", 2, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()))),
      Ns(3, "Ref2", "Ref2", None, None, List(
        Attr(1, "str2", 1, "String", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, List()),
        Attr(2, "int2", 1, "Int", None, None, Some(Set("uniqueValue")), None, None, None, None, None, List()),
        Attr(3, "enum2", 1, "String", Some(Set("enum22", "enum20", "enum21")), None, None, None, None, None, None, None, List()),
        Attr(4, "strs2", 2, "String", None, None, None, None, None, None, None, None, List()),
        Attr(5, "ints2", 2, "Int", None, None, Some(Set("noHistory")), None, None, None, None, None, List())))))
  ))
}


class PartitionSetup extends Schema with Settings {
  ResetDbs.resetDbs(Seq("CoreTest", "Partition"))
  val moleculeAdminConn = Conn("datomic:free://localhost:4334/MoleculeAdmin")
  val coreConn          = Conn("datomic:free://localhost:4334/CoreTest")
  val partitionConn     = Conn("datomic:free://localhost:4334/Partition")
}

class PartitionSetup1 extends Schema with Settings {
  ResetDbs.resetDbs(Seq("CoreTest", "Partition1"))
  val moleculeAdminConn = Conn("datomic:free://localhost:4334/MoleculeAdmin")
  val coreConn          = Conn("datomic:free://localhost:4334/CoreTest")
  val partition1Conn    = Conn("datomic:free://localhost:4334/Partition1")
}
