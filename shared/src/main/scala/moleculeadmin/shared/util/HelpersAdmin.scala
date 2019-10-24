package moleculeadmin.shared.util

import java.net.URI
import java.util.{Date, UUID}
import molecule.ast.query.{CollectionBinding, InDataSource, InVar, Query, RelationBinding}
import molecule.util.Helpers

trait HelpersAdmin extends Helpers  {

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


  def firstLow(str: Any): String = str.toString.head.toLower + str.toString.tail

  implicit class capitalized2lower(cap: String) {
    def low: String = if (cap.nonEmpty) firstLow(cap) else ""
  }

//  def cleanAttr(attr: String): String = attr.last match {
//    case '_' => attr.init
//    case '$' => attr.init
//    case _   => attr
//  }

//  protected def cast(value: Any): String = value match {
//    case (a, b)                             => s"(${cast(a)}, ${cast(b)})"
//    case v: Long                            => v + "L"
//    case v: Float                           => v + "f"
//    case date: Date                         => "\"" + date2str(date) + "\""
//    case v: String if v.startsWith("__n__") => v.drop(5)
//    case v: String                          => "\"" + v + "\""
//    case v: UUID                            => "\"" + v + "\""
//    case v: URI                             => "\"" + v + "\""
//    case v                                  => v.toString
//  }

//  final protected def os(opt: Option[Set[_]]): String = if (opt.isEmpty) "None" else s"""Some(${opt.get.map(cast)})"""

  def renderValue(v: Any): String = v match {
    case d: Date                            => date2str(d.asInstanceOf[Date])
    case s: String if s.startsWith("__n__") => s.drop(5)
    case _                                  => v.toString
  }

  implicit class StrIsX(str: String) {
    def isInt = scala.util.Try(str.toInt).isSuccess
    def isLong = scala.util.Try(str.toLong).isSuccess
    def isDouble = scala.util.Try(str.toDouble).isSuccess
    def isFloat = scala.util.Try(str.toFloat).isSuccess
    def isBigInt = scala.util.Try(BigInt(str)).isSuccess
    def isBigDecimal = scala.util.Try(BigDecimal(str)).isSuccess
    def isBoolean = scala.util.Try(str.toBoolean).isSuccess
  }


  class Timer {
    val time0 = System.currentTimeMillis
    var time1 = time0
    //    println("timer start ---------------------------------")

    def log(n: Int): Unit = {
      val time2 = System.currentTimeMillis - time1
      println(s"time $n: " + "%10d".format(time2))
      time1 = System.currentTimeMillis()
    }
    def total: String = (System.currentTimeMillis - time0).toString
      .reverse.grouped(3).mkString(" ").reverse + " ms"

  }

  def prettyMillisDelta(millisDelta: Long): String = {
    val second = 1000L
    val minute = second * 60
    val hour   = minute * 60
    val day    = hour * 24
    val month  = day * 30
    val year   = day * 365

    if (millisDelta / year > 1) millisDelta / year + "years ago"
    else if (millisDelta / year == 1) "1 year"
    else if (millisDelta / month > 1) millisDelta / month + "months ago"
    else "xxx"
  }

  def thousands(i: Long): String =
    i.toString.reverse.grouped(3).mkString(" ").reverse


  // Todo - hack to transmit input types because we can't transfer them typed with boopickle - maybe use upickle instead here?
  def c(arg: Any): String = arg match {
    case _: String     => "String"
    case _: Int        => "Long"
    case _: Long       => "Long"
    case _: Float      => "Double"
    case _: Double     => "Double"
    case _: BigInt     => "BigInt"
    case _: BigDecimal => "BigDecimal"
    case _: Boolean    => "Boolean"
    case _: Date       => "Date"
    case _: UUID       => "UUID"
    case _: URI        => "URI"
  }

  def encodeInputs(q: Query): (
    Seq[(Int, (String, String))],
      Seq[(Int, Seq[(String, String)])],
      Seq[(Int, Seq[Seq[(String, String)]])]
    ) = q.i.inputs.zipWithIndex.foldLeft(
    Seq.empty[(Int, (String, String))],
    Seq.empty[(Int, Seq[(String, String)])],
    Seq.empty[(Int, Seq[Seq[(String, String)]])]
  ) {
    case ((l, ll, lll), (InVar(RelationBinding(_), argss), i))   => (l, ll, lll :+ (i, argss.map(v => v.map(w => (c(w), w.toString)))))
    case ((l, ll, lll), (InVar(CollectionBinding(_), argss), i)) => (l, ll :+ (i, argss.head.map(v => (c(v), v.toString))), lll)
    case ((l, ll, lll), (InVar(_, argss), i))                    => (l :+ (i, (c(argss.head.head), argss.head.head.toString)), ll, lll)
    case ((l, ll, lll), (InDataSource(_, argss), i))             => (l :+ (i, (c(argss.head.head), argss.head.head.toString)), ll, lll)
    case other                                                   => sys.error(s"[molecule.ops.QueryOps] UNEXPECTED inputs: $other")
  }

}
