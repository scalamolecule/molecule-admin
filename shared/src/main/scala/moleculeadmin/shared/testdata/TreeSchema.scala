package moleculeadmin.shared.testdata

import moleculeadmin.shared.ast.schema._

trait TreeSchema {

  implicit val nsMap: Map[String, Ns] = Map(
    "Aaa" -> Ns(1, "Aaa", "Aaa", None, None, List(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
      Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
    "Bbb" -> Ns(2, "Bbb", "Bbb", None, None, List(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
      Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
    "Ccc" -> Ns(3, "Ccc", "Ccc", None, None, List(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
      Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
    "Ddd" -> Ns(4, "Ddd", "Ddd", None, None, List(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
      Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
      Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil))),


    "Ns" -> Ns(1, "Ns", "Ns", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "str", 1, "String", None, None, Some(Set("fulltext")), Some("Card one String attribute"), None, None, None, None, Seq()),
      Attr(2, "int", 1, "Int", None, None, None, Some("Card one Int attribute"), None, None, None, None, Seq()),
      Attr(3, "long", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(4, "float", 1, "Float", None, None, None, None, None, None, None, None, Seq()),
      Attr(5, "double", 1, "Double", None, None, None, None, None, None, None, None, Seq()),
      Attr(6, "bool", 1, "Boolean", None, None, None, None, None, None, None, None, Seq()),
      Attr(7, "bigInt", 1, "BigInt", None, None, None, None, None, None, None, None, Seq()),
      Attr(8, "bigDec", 1, "BigDecimal", None, None, None, None, None, None, None, None, Seq()),
      Attr(9, "date", 1, "Date", None, None, None, None, None, None, None, None, Seq()),
      Attr(10, "uuid", 1, "UUID", None, None, None, None, None, None, None, None, Seq()),
      Attr(11, "uri", 1, "URI", None, None, None, None, None, None, None, None, Seq()),
      Attr(12, "enum", 1, "String", Some(Set("enum8", "enum2", "enum7", "enum1", "enum5", "enum3", "enum4", "enum0", "enum9", "enum6")), None, None, None, None, None, None, None, Seq()),
      Attr(13, "parent", 1, "ref", None, Some("Ns"), None, None, None, None, None, None, Seq()),
      Attr(14, "ref1", 1, "ref", None, Some("Ref1"), None, None, None, None, None, None, Seq()),
      Attr(15, "refSub1", 1, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, Seq()),
      Attr(16, "strs", 2, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, Seq()),
      Attr(17, "ints", 2, "Int", None, None, None, None, None, None, None, None, Seq()),
      Attr(18, "longs", 2, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(19, "floats", 2, "Float", None, None, None, None, None, None, None, None, Seq()),
      Attr(20, "doubles", 2, "Double", None, None, None, None, None, None, None, None, Seq()),
      Attr(21, "bools", 2, "Boolean", None, None, None, None, None, None, None, None, Seq()),
      Attr(22, "bigInts", 2, "BigInt", None, None, None, None, None, None, None, None, Seq()),
      Attr(23, "bigDecs", 2, "BigDecimal", None, None, None, None, None, None, None, None, Seq()),
      Attr(24, "dates", 2, "Date", None, None, None, None, None, None, None, None, Seq()),
      Attr(25, "uuids", 2, "UUID", None, None, None, None, None, None, None, None, Seq()),
      Attr(26, "uris", 2, "URI", None, None, None, None, None, None, None, None, Seq()),
      Attr(27, "enums", 2, "String", Some(Set("enum8", "enum2", "enum7", "enum1", "enum5", "enum3", "enum4", "enum0", "enum9", "enum6")), None, None, None, None, None, None, None, Seq()),
      Attr(28, "parents", 2, "ref", None, Some("Ns"), None, None, None, None, None, None, Seq()),
      Attr(29, "refs1", 2, "ref", None, Some("Ref1"), None, None, None, None, None, None, Seq()),
      Attr(30, "refsSub1", 2, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, Seq()),
      Attr(31, "strMap", 3, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, Seq()),
      Attr(32, "intMap", 3, "Int", None, None, None, None, None, None, None, None, Seq()),
      Attr(33, "longMap", 3, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(34, "floatMap", 3, "Float", None, None, None, None, None, None, None, None, Seq()),
      Attr(35, "doubleMap", 3, "Double", None, None, None, None, None, None, None, None, Seq()),
      Attr(36, "boolMap", 3, "Boolean", None, None, None, None, None, None, None, None, Seq()),
      Attr(37, "bigIntMap", 3, "BigInt", None, None, None, None, None, None, None, None, Seq()),
      Attr(38, "bigDecMap", 3, "BigDecimal", None, None, None, None, None, None, None, None, Seq()),
      Attr(39, "dateMap", 3, "Date", None, None, None, None, None, None, None, None, Seq()),
      Attr(40, "uuidMap", 3, "UUID", None, None, None, None, None, None, None, None, Seq()),
      Attr(41, "uriMap", 3, "URI", None, None, None, None, None, None, None, None, Seq()))),
    "Ref1" -> Ns(2, "Ref1", "Ref1", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "str1", 1, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(2, "int1", 1, "Int", None, None, None, None, None, None, None, None, Seq()),
      Attr(3, "enum1", 1, "String", Some(Set("enum12", "enum10", "enum11")), None, None, None, None, None, None, None, Seq()),
      Attr(4, "ref2", 1, "ref", None, Some("Ref2"), None, None, None, None, None, None, Seq()),
      Attr(5, "refSub2", 1, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, Seq()),
      Attr(6, "strs1", 2, "String", None, None, None, None, Some(""), None, None, None, Seq()),
      Attr(7, "ints1", 2, "Int", None, None, None, None, None, None, None, None, Seq()),
      Attr(8, "refs2", 2, "ref", None, Some("Ref2"), None, None, None, None, None, None, Seq()),
      Attr(9, "refsSub2", 2, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, Seq()))),
    "Ref2" -> Ns(3, "Ref2", "Ref2", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "str2", 1, "String", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, Seq()),
      Attr(2, "int2", 1, "Int", None, None, Some(Set("uniqueValue")), None, None, None, None, None, Seq()),
      Attr(3, "enum2", 1, "String", Some(Set("enum22", "enum20", "enum21")), None, None, None, None, None, None, None, Seq()),
      Attr(4, "strs2", 2, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(5, "ints2", 2, "Int", None, None, Some(Set("noHistory")), None, None, None, None, None, Seq())))
  )
}
