package moleculeadmin.client.app.logic.query.data.groupEdit.compileTest

import java.time.LocalDateTime
import moleculeadmin.client.app.logic.query.QueryState.columns
import moleculeadmin.client.app.logic.query.data.groupEdit.ops.ScalaCode
import moleculeadmin.client.scalafiddle.ScalaFiddle
import moleculeadmin.shared.ast.query.Col
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters._


object Card2 extends TestScalaFiddle {

  case class Compiler(
    col: Col,
    testData: List[(List[String], String, String)]
  ) extends BaseTestGroupEdit(col) {

    val (attrOpt, attrClean) = if (optional)
      (attr, attr.init) else (attr + "$", attr)

    testData.foreach {
      case (attrValues, expected, rhs0) => {

        // Use optional notation with '$' in all tested rhs
        val rhs = rhs0.replace(attrOpt, attrClean)

        val scalaCode: String = ScalaCode(columns.now, col, rhs).get
        //        println(scalaCode)

        ScalaFiddle[js.Array[String]](scalaCode).lambda2.foreach { lambda =>
          val (newValues, error) = lambda(eid, attrValues.toJSArray) match {
            case js.Tuple2(vs, "") if vs.isEmpty => (Nil, "")
            case js.Tuple2(vs, "")               => (vs.toList, "")
            case js.Tuple2(_, error)             => (Nil, error)
          }
          showResult(rhs, attrValues.toString, newValues.toString, expected, error, scalaCode)
        }
      }
    }
  }

  // No need to check both mandatory and optional attr values since they are both
  // conflated to a list that can be empty or contain values.

  def ints(): Unit = {
    val testData = List(
      // Only Int's accepted.
      //      (Nil, "List()", "List(2L)"),
      //      (Nil, "List()", "List(BigInt(2))"),
      //      (Nil, "List()", "List(1.1f)"),
      //      (Nil, "List()", "List(1.1)"),
      //      (Nil, "List()", "List(BigDecimal(1.1))"),

      (Nil, "List()", ""),
      (Nil, "List()", "Nil"),
      (Nil, "List()", "Seq()"),
      (Nil, "List()", "Set()"),
      (Nil, "List()", "List()"),

      (Nil, "List(1)", "List(1)"),

      (List("1"), "List(1)", "ints"),
      (List("1"), "List(2)", "List(ints.head + 1)"),

      (List("1"), "List(1, 2)", "ints :+ 2"),

      // We can pattern match Int attr against Int value since Int's are not converted to BigInt
      (List("1"), "List(2)",
        """ints match {
          |  case List(1) => List(2)
          |  case _       => Nil
          |}""".stripMargin),

      (List("1"), "List()",
        """ints match {
          |  case vs if vs.contains(42) => List(2)
          |  case _                     => Nil
          |}""".stripMargin),

      (Nil, "List(2)",
        """ints match {
          |  case Nil => List(2)
          |  case vs  => List(0)
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "ints", "Int", "listDouble", 2), testData)
  }


  def longs(): Unit = {
    val testData = List(
      // Floating point number types non-compatible
      //      (Nil, "List(1.1)", "List(1.1f)"),
      //      (Nil, "List(1.1)", "List(1.1)"),
      //      (Nil, "List(1.1)", "List(BigDecimal(1.1))"),

      (Nil, "List()", ""),
      (Nil, "List()", "Nil"),
      (Nil, "List()", "Seq()"),
      (Nil, "List()", "Set()"),
      (Nil, "List()", "List()"),

      (Nil, "List(1)", "List(1)"),
      (Nil, "List(1)", "List(1L)"),
      (Nil, "List(1)", "List(e)"),
      (Nil, "List(1)", "List(BigInt(1))"),

      (List("1"), "List(2)", "List(longs.head + 1)"),
      (List("1"), "List(2)", "List(longs.head + 1L)"),
      (List("1"), "List(2)", "List(longs.head + e)"),
      (List("1"), "List(2)", "List(longs.head + BigInt(1))"),

      (List("1"), "List(1, 2)", "longs :+ 2"),
      (List("1"), "List(1, 2)", "longs :+ 2L"),
      (List("1"), "List(1, 2)", "longs :+ BigInt(2)"),
      (List("1"), "List(1, 2)", "longs :+ (e + 1)"),

      // Converted attr `longs` can't pattern match directly on
      // non-converted value `List(2)`
      //      (List("2"), "List(6)",
      //        """longs match {
      //          |  case List(2) => List(6)
      //          |  case vs      => vs
      //          |}""".stripMargin),

      // Instead, compare implicitly converted value
      (List("2"), "List(6)",
        """longs match {
          |  case List(v) if v == 2 => List(6)
          |  case vs                => vs
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "longs", "Long", "listDouble", 2), testData)
  }


  def bigInts(): Unit = {
    val testData = List(
      // Floating point number types non-compatible
      //      (Nil, "List(1.1)", "List(1.1f)"),
      //      (Nil, "List(1.1)", "List(1.1)"),
      //      (Nil, "List(1.1)", "List(BigDecimal(1.1))"),

      (Nil, "List()", ""),
      (Nil, "List()", "Nil"),
      (Nil, "List()", "Seq()"),
      (Nil, "List()", "Set()"),
      (Nil, "List()", "List()"),

      (Nil, "List(1)", "List(1)"),
      (Nil, "List(1)", "List(1L)"),
      (Nil, "List(1)", "List(e)"),
      (Nil, "List(1)", "List(BigInt(1))"),

      (List("1"), "List(2)", "List(bigInts.head + 1)"),
      (List("1"), "List(2)", "List(bigInts.head + 1L)"),
      (List("1"), "List(2)", "List(bigInts.head + e)"),
      (List("1"), "List(2)", "List(bigInts.head + BigInt(1))"),

      (List("1"), "List(1, 2)", "bigInts :+ 2"),
      (List("1"), "List(1, 2)", "bigInts :+ 2L"),
      (List("1"), "List(1, 2)", "bigInts :+ BigInt(2)"),
      (List("1"), "List(1, 2)", "bigInts :+ (e + 1)"),

      //      // Converted attr can't pattern match non-converted value directly
      //      (List("2"), "List(6)",
      //        """bigInts match {
      //          |  case List(2) => List(6)
      //          |  case vs      => vs
      //          |}""".stripMargin),

      // Instead, compare implicitly converted value
      (List("2"), "List(6)",
        """bigInts match {
          |  case List(v) if v == 2 => List(6)
          |  case vs                => vs
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bigInts", "BigInt", "listDouble", 2), testData)
  }

  BigInt(1)


  def floats(): Unit = {
    val testData = List(
      // Use Double instead of Float
      (Nil, s"$iae Please use Double instead of Float in expression `List(4.2f)` to get correct floating point precision.", "List(4.2f)"),

      (Nil, "List()", ""),
      (Nil, "List()", "Nil"),
      (Nil, "List()", "Seq()"),
      (Nil, "List()", "Set()"),
      (Nil, "List()", "List()"),
      (Nil, "List(1)", "List(1)"),

      (Nil, "List(1)", "List(1)"),
      (Nil, "List(1)", "List(1L)"),
      (Nil, "List(1)", "List(e)"),
      (Nil, "List(1)", "List(BigInt(1))"),
      (Nil, "List(1.4)", "List(1.4)"),
      (Nil, "List(1.4)", "List(BigDecimal(1.4))"),

      (List("1.1"), "List(2.1)", "List(floats.head + 1)"),
      (List("1.1"), "List(2.1)", "List(floats.head + 1L)"),
      (List("1.1"), "List(2.1)", "List(floats.head + e)"),
      (List("1.1"), "List(2.1)", "List(floats.head + BigInt(1))"),
      (List("1.1"), "List(2.4)", "List(floats.head + 1.3)"),
      (List("1.1"), "List(2.4)", "List(floats.head + BigDecimal(1.3))"),


      (List("1.1"), "List(1.1, 1)", "floats :+ 1"),
      (List("1.1"), "List(1.1, 1)", "floats :+ 1L"),
      (List("1.1"), "List(1.1, 1)", "floats :+ e"),
      (List("1.1"), "List(1.1, 1)", "floats :+ BigInt(1)"),
      (List("1.1"), "List(1.1, 1.3)", "floats :+ 1.3"),
      (List("1.1"), "List(1.1, 1.3)", "floats :+ BigDecimal(1.3)"),

      //      // Converted attr can't pattern match non-converted value directly
      //      (List("1.1"), "List(2.3)",
      //        """floats match {
      //          |  case List(1.1f) => List(2.3)
      //          |  case vs         => vs
      //          |}""".stripMargin),

      // Instead, compare implicitly converted value
      (List("1.1"), "List(2.3)",
        """floats match {
          |  case vs if vs.contains(1.1) => List(2.3)
          |  case vs                     => vs
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "floats", "Float", "listDouble", 2), testData)
  }


  def doubles(): Unit = {
    val testData = List(
      // Use Double instead of Float
      (Nil, s"$iae Please use Double instead of Float in expression `List(4.2f)` to get correct floating point precision.", "List(4.2f)"),

      (Nil, "List()", ""),
      (Nil, "List()", "Nil"),
      (Nil, "List()", "Seq()"),
      (Nil, "List()", "Set()"),
      (Nil, "List()", "List()"),
      (Nil, "List(1)", "List(1)"),

      (Nil, "List(1)", "List(1)"),
      (Nil, "List(1)", "List(1L)"),
      (Nil, "List(1)", "List(e)"),
      (Nil, "List(1)", "List(BigInt(1))"),
      (Nil, "List(1.4)", "List(1.4)"),
      (Nil, "List(1.4)", "List(BigDecimal(1.4))"),

      (List("1.1"), "List(2.1)", "List(doubles.head + 1)"),
      (List("1.1"), "List(2.1)", "List(doubles.head + 1L)"),
      (List("1.1"), "List(2.1)", "List(doubles.head + e)"),
      (List("1.1"), "List(2.1)", "List(doubles.head + BigInt(1))"),
      (List("1.1"), "List(2.4)", "List(doubles.head + 1.3)"),
      (List("1.1"), "List(2.4)", "List(doubles.head + BigDecimal(1.3))"),

      (List("1.1"), "List(1.1, 1)", "doubles :+ 1"),
      (List("1.1"), "List(1.1, 1)", "doubles :+ 1L"),
      (List("1.1"), "List(1.1, 1)", "doubles :+ e"),
      (List("1.1"), "List(1.1, 1)", "doubles :+ BigInt(1)"),
      (List("1.1"), "List(1.1, 1.3)", "doubles :+ 1.3"),
      (List("1.1"), "List(1.1, 2.3)", "doubles :+ 2.3"),
      (List("1.1"), "List(1.1, 2.3)", "doubles :+ BigDecimal(2.3)"),
      //
      //      //      // Converted attr can't pattern match non-converted value directly
      //      //      (List("1.1"), "List(2.3)",
      //      //        """doubles match {
      //      //          |  case List(1.1f) => List(2.3)
      //      //          |  case vs         => vs
      //      //          |}""".stripMargin),
      //
      //      // Instead, compare implicitly converted value
      //      (List("1.1"), "List(2.3)",
      //        """doubles match {
      //          |  case vs if vs.last == 1.1 => List(2.3)
      //          |  case vs                   => vs
      //          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "doubles", "Double", "listDouble", 2), testData)
  }


  def bigDec(): Unit = {
    val testData = List(
      // Use Double instead of Float
      (Nil, s"$iae Please use Double instead of Float in expression `List(4.2f)` to get correct floating point precision.", "List(4.2f)"),

      (Nil, "List()", ""),
      (Nil, "List()", "Nil"),
      (Nil, "List()", "Seq()"),
      (Nil, "List()", "Set()"),
      (Nil, "List()", "List()"),
      (Nil, "List(1)", "List(1)"),

      (Nil, "List(1)", "List(1)"),
      (Nil, "List(1)", "List(1L)"),
      (Nil, "List(1)", "List(e)"),
      (Nil, "List(1)", "List(BigInt(1))"),
      (Nil, "List(1.4)", "List(1.4)"),
      (Nil, "List(1.4)", "List(BigDecimal(1.4))"),

      (List("1.1"), "List(2.1)", "List(bigDecs.head + 1)"),
      (List("1.1"), "List(2.1)", "List(bigDecs.head + 1L)"),
      (List("1.1"), "List(2.1)", "List(bigDecs.head + e)"),
      (List("1.1"), "List(2.1)", "List(bigDecs.head + BigInt(1))"),
      (List("1.1"), "List(2.4)", "List(bigDecs.head + 1.3)"),
      (List("1.1"), "List(2.4)", "List(bigDecs.head + BigDecimal(1.3))"),

      (List("1.1"), "List(1.1, 1)", "bigDecs :+ 1"),
      (List("1.1"), "List(1.1, 1)", "bigDecs :+ 1L"),
      (List("1.1"), "List(1.1, 1)", "bigDecs :+ e"),
      (List("1.1"), "List(1.1, 1)", "bigDecs :+ BigInt(1)"),
      (List("1.1"), "List(1.1, 1.3)", "bigDecs :+ 1.3"),
      (List("1.1"), "List(1.1, 2.3)", "bigDecs :+ 2.3"),
      (List("1.1"), "List(1.1, 2.3)", "bigDecs :+ BigDecimal(2.3)"),

      //      // Converted attr can't pattern match non-converted value directly
      //      (List("1.1"), "List(2.3)",
      //        """bigDecs match {
      //          |  case List(1.1f) => List(2.3)
      //          |  case vs         => vs
      //          |}""".stripMargin),

      // Instead, compare implicitly converted value
      (List("1.1"), "List(2.3)",
        """bigDecs match {
          |  case vs if vs.head == 1.1 => List(2.3)
          |  case vs                   => vs
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bigDecs", "BigDecimal", "listDouble", 2), testData)
  }


  def strings(): Unit = {
    val testData = List(
      // Options not used for card many lists. Instead a list can be either
      // empty (~ None) or having values (~ Some(list)).
      // Use mandatory notation without '$' postfix ($ is stripped):
      (List("a"), "List(a)", "strs$"),
      // Treated same as
      (List("a"), "List(a)", "strs"),

      // Unchanged
      (Nil, "List()", ""),
      (Nil, "List()", "Nil"),
      (Nil, "List()", "Seq()"),
      (Nil, "List()", "List()"),
      (List("a"), "List(a)", "strs"),
      (List("a", "b"), "List(a, b)", "strs"),

      // Add
      (Nil, "List(a)", """strs :+ "a""""),
      (List("a"), "List(a, b)", """strs :+ "b""""),

      // Update
      (List("a"), "List(ax)", """strs.map(v => v + "x")"""),
      (List("a", "b"), "List(ax, bx)", """strs.map(v => v + "x")"""),
      (List("a"), "List(a, x, y)", """strs.flatMap(v => List(v, "x", "y"))"""),

      // Retract element
      (List("a", "b", "c"), "List(a, b)", """strs.init"""),
      (List("a", "b", "c"), "List(b, c)", """strs.tail"""),

      // Retract all
      (List("a", "b"), "List()", ""),
      (List("a", "b"), "List()", "Nil"),
      (List("a", "b"), "List()", "List()"),
      (List("a", "b"), "List()", "strs.take(0)"),

      // String treatment
      (List("a2"), "List()", """List("")"""),
      (List("a2"), "List( )", """List(" ")"""),
      (List("a2"), "List(\n )", """List("\n ")"""),
      (List("a2"), "List( \n )", """List(" \n ")"""),
      (List("a3"), "List(Code is:\na3)", """List("Code is:\n" + strs.head)"""),
      (List("a3"), "List(a34)", """List(strs.head + 4)"""),

      (List("a3"), "List(a3, b)", """strs :+ "b""""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "strs", "String", "listString", 2), testData)
  }


  def bools(): Unit = {
    val testData = List(
      (List("true"), "List(false)", "List(false)"),
      (List("true"), "List(true)", "bools"),
      (List("true"), "List(false)", "List(!bools.head)"),
      (List("true"), "List(false)", "bools.map(!_)"),
      (List("true"), "List(false)", "List(bools.head && 7 == 8)"),
      (List("true"), "List(true)", "List(bools.head || 7 == 8)"),
      (List("true"), "List(true, false)", "bools :+ false"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bools", "Boolean", "listString", 2), testData)
  }


  def dates(): Unit = {
    val oneDate = List(LocalDateTime.of(2001, 3, 5, 7, 9, 11).toString)

    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "List()"),
      (Nil, "List()", "dates$"),

      // For more examples, see card 1 date examples

      // 3 ways of constructing dates
      (Nil, "List(2001-03-05T07:09:11)", "List(LocalDateTime.of(2001, 3, 5, 7, 9, 11))"),
      (Nil, "List(2001-03-05T07:09:11)", "List(d(2001, 3, 5, 7, 9, 11))"),
      // Implicitly creating LocalDateTime from String
      (Nil, "List(2001-03-05T07:09:11)", """List("2001-3-5 7:9:11")"""),

      (oneDate, "List(2001-03-05T07:09:11)", "dates"),
      (oneDate, "List(2001-03-05T07:09:11)", "List(dates.head)"),
      (oneDate, "List(2001-03-05T07:09:12)", "List(dates.head.plusSeconds(1))"),

      (oneDate, "List(2001-03-05T07:09:11, 2002-01-01T00:00)", "dates :+ LocalDateTime.of(2002, 1, 1, 0, 0)"),
      (oneDate, "List(2001-03-05T07:09:11, 2002-01-01T00:00)", "dates :+ d(2002)"),
      (oneDate, "List(2001-03-05T07:09:11, 2002-01-01T00:00)", """dates :+ "2002""""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "dates", "Date", "listString", 2), testData)
  }


  def uuids(): Unit = {
    val uuid1    = "aba20f8e-8e79-475c-b8d1-df11f57b29ba"
    val uuid2    = "b0924388-9c1b-4ce4-92ac-7b8d2b01beec"
    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "List()"),

      (List(uuid1), s"List($uuid1)", "uuids"),
      (List(uuid2), s"List($uuid2)", s"""List(UUID.fromString("$uuid2"))"""),
      // Implicitly creating UUID from String
      (List(uuid2), s"List($uuid2)", s"""List("$uuid2")"""),

      (List(uuid1), s"List($uuid1, $uuid2)", s"""uuids :+ UUID.fromString("$uuid2")"""),
      (List(uuid1), s"List($uuid1, $uuid2)", s"""uuids :+ "$uuid2""""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uuids", "UUID", "listString", 2), testData)
  }


  def uris(): Unit = {
    val uri1     = "uri1"
    val uri2     = "uri2"
    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "List()"),

      (List(uri1), s"List(uri1)", "uris"),
      (List(uri2), s"List(uri2)", """List(new URI("uri2"))"""),
      // Implicitly creating URI from String
      (List(uri2), s"List(uri2)", """List("uri2")"""),

      (List(uri1), s"List(uri1, uri2)", s"""uris :+ new URI("uri2")"""),
      (List(uri1), s"List(uri1, uri2)", s"""uris :+ "uri2""""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uris", "URI", "listString", 2), testData)
  }
}