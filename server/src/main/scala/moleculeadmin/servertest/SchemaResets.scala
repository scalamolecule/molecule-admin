package moleculeadmin.servertest

import ammonite.ops._
import db.DatomicUri
import moleculeadmin.shared.ast.metaSchema._


trait SchemaResets extends DatomicUri {

  val projectHome: Path = if (pwd.last == "server") pwd else pwd / "server"

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

  val partitionDefFilePath = projectHome / "src" / "main" / "scala" / "db" / "migration" / "schema" / "PartitionDefinition.scala"

  val partitionMetaSchema = MetaSchema(List(
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

  val partition1DefFilePath = projectHome / "src" / "main" / "scala" / "db" / "migration" / "schema" / "Partition1Definition.scala"

  val partition1MetaSchema = MetaSchema(List(
    MetaPart(1, "a", None, None, List(
      MetaNs(1, "Aa", "a_Aa", None, None, List()))),
    MetaPart(2, "b", None, None, List(
      MetaNs(1, "Bb", "b_Bb", None, None, List(
        MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
        MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
        MetaAttr(3, "bb3", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
        MetaAttr(4, "bb4", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
      )),
      MetaNs(2, "Bc", "b_Bc", None, None, List(
        MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
      MetaNs(3, "Bd", "b_Bd", None, None, List(
        MetaAttr(1, "bd1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
    )),
    MetaPart(3, "c", None, None, List()),
  ))

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

  val coreDefFilePath = projectHome / "src" / "main" / "scala" / "db" / "core" / "schema" / "CoreTestDefinition.scala"


  val coreMetaSchema = MetaSchema(Seq(
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
        MetaAttr(5, "ints2", 2, "Int", Nil, None, Seq("noHistory"), None, None, None, None, None, List())))))
  ))
}
