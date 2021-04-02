package moleculeadmin.shared.testdata

import moleculeadmin.shared.ast.metaSchema._

trait TreeSchema {

  implicit val nsMap: Map[String, MetaNs] = Map(
    "Aaa" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Nil),
      MetaAttr(1, "attrA", 1, "String", Nil, None, Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(2, "ab", 1, "ref", Nil, Some("Bbb"), Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(3, "ac", 1, "ref", Nil, Some("Ccc"), Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(4, "ad", 1, "ref", Nil, Some("Ddd"), Seq("indexed"), None, None, None, None, None, Nil))),
    "Bbb" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Nil),
      MetaAttr(1, "attrB", 1, "String", Nil, None, Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(2, "ba", 1, "ref", Nil, Some("Aaa"), Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(3, "bc", 1, "ref", Nil, Some("Ccc"), Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(4, "bd", 1, "ref", Nil, Some("Ddd"), Seq("indexed"), None, None, None, None, None, Nil))),
    "Ccc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Nil),
      MetaAttr(1, "attrC", 1, "String", Nil, None, Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(2, "ca", 1, "ref", Nil, Some("Aaa"), Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(3, "cb", 1, "ref", Nil, Some("Bbb"), Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(4, "cd", 1, "ref", Nil, Some("Ddd"), Seq("indexed"), None, None, None, None, None, Nil))),
    "Ddd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Nil),
      MetaAttr(1, "attrD", 1, "String", Nil, None, Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(2, "da", 1, "ref", Nil, Some("Aaa"), Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(3, "db", 1, "ref", Nil, Some("Bbb"), Seq("indexed"), None, None, None, None, None, Nil),
      MetaAttr(4, "dc", 1, "ref", Nil, Some("Ccc"), Seq("indexed"), None, None, None, None, None, Nil))),


    "Ns" -> MetaNs(1, "Ns", "Ns", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "str", 1, "String", Nil, None, Seq("fulltext"), Some("Card one String attribute"), None, None, None, None, Seq()),
      MetaAttr(2, "int", 1, "Int", Nil, None, Nil, Some("Card one Int attribute"), None, None, None, None, Seq()),
      MetaAttr(3, "long", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(4, "float", 1, "Float", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(5, "double", 1, "Double", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(6, "bool", 1, "Boolean", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(7, "bigInt", 1, "BigInt", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(8, "bigDec", 1, "BigDecimal", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(9, "date", 1, "Date", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(10, "uuid", 1, "UUID", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(11, "uri", 1, "URI", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(12, "enum", 1, "String", Seq("enum8", "enum2", "enum7", "enum1", "enum5", "enum3", "enum4", "enum0", "enum9", "enum6"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(13, "parent", 1, "ref", Nil, Some("Ns"), Nil, None, None, None, None, None, Seq()),
      MetaAttr(14, "ref1", 1, "ref", Nil, Some("Ref1"), Nil, None, None, None, None, None, Seq()),
      MetaAttr(15, "refSub1", 1, "ref", Nil, Some("Ref1"), Seq("isComponent"), None, None, None, None, None, Seq()),
      MetaAttr(16, "strs", 2, "String", Nil, None, Seq("fulltext"), None, Some(""), None, None, None, Seq()),
      MetaAttr(17, "ints", 2, "Int", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(18, "longs", 2, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(19, "floats", 2, "Float", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(20, "doubles", 2, "Double", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(21, "bools", 2, "Boolean", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(22, "bigInts", 2, "BigInt", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(23, "bigDecs", 2, "BigDecimal", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(24, "dates", 2, "Date", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(25, "uuids", 2, "UUID", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(26, "uris", 2, "URI", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(27, "enums", 2, "String", Seq("enum8", "enum2", "enum7", "enum1", "enum5", "enum3", "enum4", "enum0", "enum9", "enum6"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(28, "parents", 2, "ref", Nil, Some("Ns"), Nil, None, None, None, None, None, Seq()),
      MetaAttr(29, "refs1", 2, "ref", Nil, Some("Ref1"), Nil, None, None, None, None, None, Seq()),
      MetaAttr(30, "refsSub1", 2, "ref", Nil, Some("Ref1"), Seq("isComponent"), None, None, None, None, None, Seq()),
      MetaAttr(31, "strMap", 3, "String", Nil, None, Seq("fulltext"), None, Some(""), None, None, None, Seq()),
      MetaAttr(32, "intMap", 3, "Int", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(33, "longMap", 3, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(34, "floatMap", 3, "Float", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(35, "doubleMap", 3, "Double", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(36, "boolMap", 3, "Boolean", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(37, "bigIntMap", 3, "BigInt", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(38, "bigDecMap", 3, "BigDecimal", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(39, "dateMap", 3, "Date", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(40, "uuidMap", 3, "UUID", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(41, "uriMap", 3, "URI", Nil, None, Nil, None, None, None, None, None, Seq()))),
    "Ref1" -> MetaNs(2, "Ref1", "Ref1", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "str1", 1, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(2, "int1", 1, "Int", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(3, "enum1", 1, "String", Seq("enum12", "enum10", "enum11"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(4, "ref2", 1, "ref", Nil, Some("Ref2"), Nil, None, None, None, None, None, Seq()),
      MetaAttr(5, "refSub2", 1, "ref", Nil, Some("Ref2"), Seq("isComponent"), None, None, None, None, None, Seq()),
      MetaAttr(6, "strs1", 2, "String", Nil, None, Nil, None, Some(""), None, None, None, Seq()),
      MetaAttr(7, "ints1", 2, "Int", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(8, "refs2", 2, "ref", Nil, Some("Ref2"), Nil, None, None, None, None, None, Seq()),
      MetaAttr(9, "refsSub2", 2, "ref", Nil, Some("Ref2"), Seq("isComponent"), None, None, None, None, None, Seq()))),
    "Ref2" -> MetaNs(3, "Ref2", "Ref2", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "str2", 1, "String", Nil, None, Seq("uniqueIdentity"), None, None, None, None, None, Seq()),
      MetaAttr(2, "int2", 1, "Int", Nil, None, Seq("uniqueValue"), None, None, None, None, None, Seq()),
      MetaAttr(3, "enum2", 1, "String", Seq("enum22", "enum20", "enum21"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(4, "strs2", 2, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(5, "ints2", 2, "Int", Nil, None, Seq("noHistory"), None, None, None, None, None, Seq())))
  )
}
