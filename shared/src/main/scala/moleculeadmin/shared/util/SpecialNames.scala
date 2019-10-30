package moleculeadmin.shared.util

import java.net.URI
import java.util.{Date, UUID}
import molecule.ast.query.{CollectionBinding, InDataSource, InVar, Query, RelationBinding}
import molecule.util.Helpers

trait SpecialNames {

  val scalaKeywords = List("abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally",
    "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private",
    "protected", "return", "sealed", "super", "this", "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield")

  val datomicFulltextNonIndexed = Seq(
    "a", "an", "and", "are", "as", "at", "be", "but", "by",
    "for", "if", "in", "into", "is", "it",
    "no", "not", "of", "on", "or", "such",
    "that", "the", "their", "then", "there", "these",
    "they", "this", "to", "was", "will", "with"
  )

  val reservedPartitionNames = List("tx", "db")

  val reservedAttrNames = List(
    "a",
    "apply",
    "assert",
    "contains",
    "e",
    "insert",
    "k",
    "not",
    "op",
    "replace",
    "retract",
    "save",
    "self",
    "t",
    "tx",
    "txInstant",
    "update",
    "v",
  )

  val reservedAttrNames2 = Seq(
    // "e" ok
    "a",
    "v",
    //    "t",
    //    "tx",
    //    "txInstant",
    "op",

    "a_",
    "v_",
    "t_",
    "tx_",
    "txInstant_",
    "op_",

    "k",
    "self",
    "index",
    "unique",
    "fulltext",
    "isComponent",
    "noHistory",
    //    "enum",
    "doc",

    "index_",
    "unique_",
    "fulltext_",
    "isComponent_",
    "noHistory_",
    //    "enum_",
    "doc_",

    "doc$",
    "index$",
    "unique$",
    "fulltext$",
    "isComponent$",
    "noHistory$",

    // Schema only
    //    "id",
    //    "part",
    //    "nsFull",
    //    "ns",
    //    "attr",
    //    "tpe",
    //    "card",
    //
    //    "id_",
    //    "part_",
    //    "nsFull_",
    //    "ns_",
    //    "attr_",
    //    "tpe_",
    //    "card_",
  )


  val cardIntStr = Map(1 -> "one", 2 -> "many", 3 -> "map")
  val cardStrInt = Map("one" -> 1, "many" -> 2, "map" -> 3)

  val tpeDatomicMolecule = Map(
    "string" -> "String",
    "long" -> "Long",
    "double" -> "Double",
    "boolean" -> "Boolean",
    "bigint" -> "BigInt",
    "bigdec" -> "BigDecimal",
    "instant" -> "Date",
    "uuid" -> "UUID",
    "uri" -> "URI",
    "bytes" -> "Byte",
    "ref" -> "ref"
  )

  val tpeMoleculeDatomic = Map(
    "String" -> "string",
    "Int" -> "long",
    "Long" -> "long",
    "Float" -> "double",
    "Double" -> "double",
    "Boolean" -> "boolean",
    "BigInt" -> "bigint",
    "BigDecimal" -> "bigdec",
    "Date" -> "instant",
    "UUID" -> "uuid",
    "URI" -> "uri",
    "Byte" -> "bytes",
    "Enum" -> "string",
    "ref" -> "ref",
    "bi" -> "ref",
    "biEdge" -> "ref",
  )

  val aggrFns       = Seq(
    "distinct",
    "count",
    // "countDistinct", // turns into "count-distinct" - handle separately
    "min",
    "max",
    "rand",
    "sample",
    "sum",
    "avg",
    "median",
    "variance",
    "stddev"
  )
  val comparisonFns = Seq("apply", ">", ">=", "<", "<=", "!=", "not", "contains")

  val allTypes = Seq("String", "Int", "Long", "Float", "Double", "BigInt", "BigDecimal", "Boolean", "Date", "UUID", "URI", "Enum", "ref", "bi", "biEdge")
  val mapTypes = Seq("String", "Int", "Long", "Float", "Double", "BigInt", "BigDecimal", "Boolean", "Date", "UUID", "URI")

  val dummy = "Dummy to keep ns open"

}
