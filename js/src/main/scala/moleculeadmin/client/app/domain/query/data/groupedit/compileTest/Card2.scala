package moleculeadmin.client.app.domain.query.data.groupedit.compileTest

import java.time.LocalDateTime
import java.util.UUID
import moleculeadmin.client.app.domain.query.data.groupedit.ops.ScalaCode
import moleculeadmin.client.scalafiddle.ScalaFiddle
import moleculeadmin.shared.ast.query.Col
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters._


object Card2 extends TestScalaFiddle {

  case class Compiler[AttrTransferType](
    col: Col, testData: List[(List[AttrTransferType], String, String)]
  ) extends BaseTestGroupEdit(col) {

    val (attrOpt, attrClean) = if (optional)
      (attr, attr.init) else (attr + "$", attr)

    testData.foreach {
      case (attrValues, expected, rhs0) => {

        // Use optional notation with '$' in all tested rhs
        val rhs = rhs0.replace(attrOpt, attrClean)

        val scalaCode: String = ScalaCode(col, rhs).get
        //        println(scalaCode)

        ScalaFiddle[js.Array[AttrTransferType]](scalaCode).lambda2.foreach { lambda =>
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


  def ints(): Unit = {
    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "List()"),
      (List(1), "List(2)", "List(ints.head + e)"),
      (List(1), "List(3)", "List(ints.head + 2)"),
      (List(1), "List(4)", "List(ints.head + BigInt(3))"),
      (List(1), "List(5)", "List(5)"),
      (List(1), "List(6)",
        """ints match {
          |  case List(1) => List(6)
          |  case vs      => vs
          |}""".stripMargin),

      (List(4), s"$iae Float not allowed in Int expression `List(4.2f)`", "List(4.2f)"),
      (List(4), s"$iae Double not allowed in Int expression `List(4.2)`", "List(4.2)"),
      (List(4), s"$iae BigDecimal not allowed in Int expression `List(BigDecimal(4.2)`)", "List(BigDecimal(4.2))"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "ints", "Int", "double", 2), testData)
  }


  def longs(): Unit = {
    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "Nil"),
      (Nil, "List()", "Seq()"),
      (Nil, "List()", "List()"),
      (List("2"), "List(3)", "List(longs.head + e)"),
      (List("2"), "List(4)", "List(longs.head + 2)"),
      (List("2"), "List(5)", "List(5)"),


      // Converted attr can't pattern match directly non-converted value
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

      //      (List("1"), s"$iae Float not allowed in Long expression `List(4.2f)`", "List(4.2f)"),
      //      (List("2"), s"$iae Double not allowed in Long expression `List(4.2)`", "List(4.2)"),
      //      (List("3"), s"$iae BigDecimal not allowed in Long expression `List(BigDecimal(4.2)`)", "List(BigDecimal(4.2))"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "longs", "Long", "double", 2), testData)
  }


  def bigInts(): Unit = {
    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "List()"),
      (List("5"), "List(6)", "List(bigInts + e)"),
      (List("5"), "List(7)", "List(bigInts + 2)"),
      (List("5"), "List(8)", "List(8)"),
      (List("5"), "List(9)",
        """bigInts match {
          |  case vs if vs.head > 3 => List(9)
          |  case vs                => vs
          |}""".stripMargin),

      (List("1"), s"$iae Float not allowed in BigInt expression `List(4.2f)`", "List(4.2f)"),
      (List("2"), s"$iae Double not allowed in BigInt expression `List(4.2)`", "List(4.2)"),
      (List("3"), s"$iae BigDecimal not allowed in BigInt expression `List(BigDecimal(4.2)`)", "List(BigDecimal(4.2))"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bigInts", "BigInt", "double", 2), testData)
  }


  def floats(): Unit = {
    val testData = List(
      //      (Nil, "List()", ""),
      //      (Nil, "List()", "Nil"),
      //      (Nil, "List()", "Seq()"),
      //      (Nil, "List()", "List()"),
      //      (List("3.1"), "List(4.1)", "List(floats.head + e)"),
      //      (List("3.1"), "List(5.1)", "List(floats.head + 2L)"),
      //      (List("3.1"), "List(6.1)", "List(floats.head + 3)"),
      //      (List("3.1"), "List(7.1)", "List(floats.head + BigInt(4))"),
      //      (List("3.1"), "List(8.2)", "List(floats.head + 5.1)"),
      //      (List("3.1"), "List(8.3)", "List(floats.head + 5.2)"),
      //      (List("3.1"), "List(8.4)", "List(floats.head + BigDecimal(5.3))"),
      //      (List("3.1"), "List(8.5)", "List(8.5)"),

      //      // Converted attr can't pattern match directly non-converted value
      //      (List("3.1"), "List(9.3)",
      //        """floats match {
      //          |  case List(3.1f) => List(9.3)
      //          |  case vs         => vs
      //          |}""".stripMargin),

      // Instead, compare implicitly converted value
      //      (List("3.1"), "List(9.3)",
      //        """floats match {
      //          |  case vs if vs.head > 3 => List(9.3)
      //          |  case vs                => vs
      //          |}""".stripMargin),
      //
      //      (List("4"), s"$iae Please use Double instead of Float in expression `List(4.2f)` to get correct floating point precision.", "List(4.2f)"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "floats", "Float", "double", 2), testData)
  }


  def doubles(): Unit = {
    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "List()"),
      (List("4.1"), "List(5.1)", "List(doubles.head + e)"),
      (List("4.1"), "List(6.1)", "List(doubles.head + 2L)"),
      (List("4.1"), "List(7.1)", "List(doubles.head + 3)"),
      (List("4.1"), "List(8.1)", "List(doubles.head + BigInt(4))"),
      (List("4.1"), "List(9.2)", "List(doubles.head + 5.1)"),
      (List("4.1"), "List(9.3)", "List(doubles.head + 5.2)"),
      (List("4.1"), "List(9.4)", "List(doubles.head + BigDecimal(5.3))"),
      (List("4.1"), "List(9.5)", "List(9.5)"),
      (List("4.1"), "List(10.3)",
        """doubles match {
          |  case vs if vs.head > 3 => List(10.3)
          |  case vs                => vs
          |}""".stripMargin),

      (List("4"), s"$iae Please use Double instead of Float in expression `List(4.2f)` to get correct floating point precision.", "List(4.2f)"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "doubles", "Double", "double", 2), testData)
  }


  def bigDecimals(): Unit = {
    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "List()"),
      (List("6.1"), "List(7.1)", "List(bigDecs + e)"),
      (List("6.1"), "List(8.1)", "List(bigDecs + 2L)"),
      (List("6.1"), "List(9.1)", "List(bigDecs + 3)"),
      (List("6.1"), "List(10.1)", "List(bigDecs + BigInt(4))"),
      (List("6.1"), "List(11.2)", "List(bigDecs + 5.1)"),
      (List("6.1"), "List(11.3)", "List(bigDecs + 5.2)"),
      (List("6.1"), "List(11.4)", "List(bigDecs + BigDecimal(5.3))"),
      (List("6.1"), "List(11.5)", "List(11.5)"),
      (List("6.1"), "List(12.2)",
        """bigDecs match {
          |  case vs if vs.head > 3 => List(12.3)
          |  case vs                => vs
          |}""".stripMargin),

      (List("4"), s"$iae Please use Double instead of Float in expression `List(4.2f)` to get correct floating point precision.", "List(4.2f)"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bigDecs", "BigDecimal", "double", 2), testData)
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
    )
    Compiler(Col(2, 0, "Ns", "Ns", "strs", "String", "string", 2), testData)
    //    Compiler(Col(2, 0, "Ns", "Ns", "strs$", "String", "string", 2), testData)
  }


  def bools(): Unit = {
    val testData = List(
      (List(true), "List(false)", "List(false)"),
      (List(true), "List(true)", "bools"),
      (List(true), "List(false)", "List(!bools.head)"),
      (List(true), "List(false)", "bools.map(!_)"),
      (List(true), "List(false)", "List(bools.head && 7 == 8)"),
      (List(true), "List(true)", "List(bools.head || 7 == 8)"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bools", "Boolean", "string", 2), testData)
  }


  def dates(): Unit = {
    val dummy = List(LocalDateTime.MIN)
    val dates = List(LocalDateTime.of(2001, 3, 5, 7, 9, 11))
    val now   = LocalDateTime.now()
    def p(s: Int, i: Int = 2): String = i match {
      case 2 => "%02d".format(s)
      case 3 => "%03d".format(s)
      case 4 => "%04d".format(s)
    }
    def y: Int = now.getYear
    def m: String = p(now.getMonthValue)
    def d: String = p(now.getDayOfMonth)
    def hh: String = p(now.getHour)
    def mm: String = p(now.getMinute)

    val testData = List(
      //      (Nil, "List()", ""),
      //      (Nil, "List()", "List()"),
      //      (Nil, "List()", "dates$"),
      (Nil, "List(2001-03-05T00:00)", """List("2001-03-05")"""),
      (List(LocalDateTime.of(2001, 3, 5, 0, 0)), "List(2001-03-05T00:00)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9)), "List(2001-03-05T07:09)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11)), "List(2001-03-05T07:09:11)", "dates"),

      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 100000000)), "List(2001-03-05T07:09:11.100)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 10000000)), "List(2001-03-05T07:09:11.010)", "dates"),
      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 1000000)), "List(2001-03-05T07:09:11.001)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 100000)), "List(2001-03-05T07:09:11.000100)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 10000)), "List(2001-03-05T07:09:11.000010)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 1000)), "List(2001-03-05T07:09:11.000001)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 100)), "List(2001-03-05T07:09:11.000000100)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 10)), "List(2001-03-05T07:09:11.000000010)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 1)), "List(2001-03-05T07:09:11.000000001)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 11)), "List(2001-03-05T07:09:11)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9, 1)), "List(2001-03-05T07:09:01)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 7, 9)), "List(2001-03-05T07:09)", "dates"),
      //      (List(LocalDateTime.of(2001, 3, 5, 0, 0)), "List(2001-03-05T00:00)", "dates"),
      //
      //      (dummy, "List(2001-03-05T07:09:11.100)", """List("2001-3-5 7:9:11.1000")"""),
      //      (dummy, "List(2001-03-05T07:09:11.100)", """List("2001-3-5 7:9:11.100")"""),
      //      (dummy, "List(2001-03-05T07:09:11.100)", """List("2001-3-5 7:9:11.10")"""),
      //      (dummy, "List(2001-03-05T07:09:11.100)", """List("2001-3-5 7:9:11.1")"""),
      //      (dummy, "List(2001-03-05T07:09:11.010)", """List("2001-3-5 7:9:11.01")"""),
      (dummy, "List(2001-03-05T07:09:11.001)", """List("2001-3-5 7:9:11.001")"""),
      //      (dummy, "List(2001-03-05T07:09:11.000100)", """List("2001-3-5 7:9:11.0001")"""),
      //      (dummy, "List(2001-03-05T07:09:11.000010)", """List("2001-3-5 7:9:11.00001")"""),
      //      (dummy, "List(2001-03-05T07:09:11.000001)", """List("2001-3-5 7:9:11.000001")"""),
      //      (dummy, "List(2001-03-05T07:09:11.000000100)", """List("2001-3-5 7:9:11.0000001")"""),
      //      (dummy, "List(2001-03-05T07:09:11.000000010)", """List("2001-3-5 7:9:11.00000001")"""),
      //      (dummy, "List(2001-03-05T07:09:11.000000001)", """List("2001-3-5 7:9:11.000000001")"""),
      //      (dummy, "List(2001-03-05T07:09:11)", """List("2001-03-05 07:09:11.000")"""),
      //      (dummy, "List(2001-03-05T07:09:11)", """List("2001-3-5 7:9:11.00")"""),
      //      (dummy, "List(2001-03-05T07:09:11)", """List("2001-3-5 7:9:11.0")"""),
      //      (dummy, "List(2001-03-05T07:09:11)", """List("2001-3-5 7:9:11")"""),
      //      (dummy, "List(2001-03-05T07:09:10)", """List("2001-3-5 7:9:10")"""),
      //      (dummy, "List(2001-03-05T07:09:01)", """List("2001-3-5 7:9:01")"""),
      //      (dummy, "List(2001-03-05T07:09:01)", """List("2001-3-5 7:9:1")"""),
      //      (dummy, "List(2001-03-05T07:09)", """List("2001-3-5 7:9:00")"""),
      //      (dummy, "List(2001-03-05T07:09)", """List("2001-3-5 7:9:0")"""),
      //      (dummy, "List(2001-03-05T07:09)", """List("2001-3-5 7:9")"""),
      //      (dummy, "List(2001-03-05T07:00)", """List("2001-3-5 7:0")"""),
      //      (dummy, "List(2001-03-05T00:00)", """List("2001-3-5 0:0")"""),
      //      (dummy, "List(2001-03-05T00:00)", """List("2001-3-5")"""),
      //
      (dates, "List(2001-03-05T07:09:11)", "dates"),
      //
      //      // Adjust backwards in time
      //      (dates, "List(2000-03-05T07:09:11)", "List(dates.head.minusYears(1))"),
      //      (dates, "List(2001-02-05T07:09:11)", "List(dates.head.minusMonths(1))"),
      //      (dates, "List(2001-02-26T07:09:11)", "List(dates.head.minusWeeks(1))"),
      (dates, "List(2001-03-04T07:09:11)", "List(dates.head.minusDays(1))"),
      //      (dates, "List(2001-03-05T06:09:11)", "List(dates.head.minusHours(1))"),
      //      (dates, "List(2001-03-05T07:08:11)", "List(dates.head.minusMinutes(1))"),
      //      (dates, "List(2001-03-05T07:09:10)", "List(dates.head.minusSeconds(1))"),
      //      (dates, "List(2001-03-05T07:09:10.900)", "List(dates.head.minusNanos(100 * 1000 * 1000))"),
      (List(LocalDateTime.of(2002, 1, 1, 0, 0)), "List(2001-12-31T23:59:59)", "List(dates.head.minusSeconds(1))"),
      //
      //      // Set time
      //      (dates, "List(2000-03-05T07:09:11)", "List(dates.head.withYear(2000))"),
      //      (dates, "List(2001-01-05T07:09:11)", "List(dates.head.withMonth(1))"),
      //      (dates, "List(2001-03-24T07:09:11)", "List(dates.head.withDayOfMonth(24))"),
      (dates, "List(2001-02-09T07:09:11)", "List(dates.head.withDayOfYear(40))"),
      //      (dates, "List(2001-03-05T01:09:11)", "List(dates.head.withHour(1))"),
      //      (dates, "List(2001-03-05T07:01:11)", "List(dates.head.withMinute(1))"),
      //      (dates, "List(2001-03-05T07:09:01)", "List(dates.head.withSecond(1))"),
      //
      //      // Adjust forward in time
      //      (dates, "List(2002-03-05T07:09:11)", "List(dates.head.plusYears(1))"),
      //      (dates, "List(2001-04-05T07:09:11)", "List(dates.head.plusMonths(1))"),
      //      (dates, "List(2001-03-12T07:09:11)", "List(dates.head.plusWeeks(1))"),
      (dates, "List(2001-03-06T07:09:11)", "List(dates.head.plusDays(1))"),
      //      (dates, "List(2001-03-05T08:09:11)", "List(dates.head.plusHours(1))"),
      //      (dates, "List(2001-03-05T07:10:11)", "List(dates.head.plusMinutes(1))"),
      //      (dates, "List(2001-03-05T07:09:12)", "List(dates.head.plusSeconds(1))"),
      //      (dates, "List(2001-03-05T07:09:11.123)", "List(dates.head.plusNanos(123 * 1000 * 1000))"),
      (List(LocalDateTime.of(2001, 12, 31, 23, 59, 59)), "List(2002-01-01T00:00)", "List(dates.head.plusSeconds(1))"),
      //
      //      // Now
      //      // Might fail if executed on each side of hour/minute change
      (dummy, s"List($y-$m-${d}T$hh:00)", "List(LocalDateTime.now().withMinute(0).withSecond(0).withNano(0))"),
      //      (dummy, s"List($y-$m-${d}T$hh:$mm)", "List(LocalDateTime.now().withSecond(0).withNano(0))"),
      //
      (List(LocalDateTime.of(2001, 7, 1, 0, 0)), "List(" + date2str(date1) + "T00:00)", "dates"),
    )

    //    Compiler(Col(2, 0, "Ns", "Ns", "dates", "Date", "string", 2), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "dates$", "Date", "string", 2), testData)
  }


  def uuids(): Unit = {
    // Use stable uuid's to allow compiler to cache
    val uuid1    = UUID.fromString("aba20f8e-8e79-475c-b8d1-df11f57b29ba")
    val uuid2    = UUID.fromString("b0924388-9c1b-4ce4-92ac-7b8d2b01beec")
    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "List()"),
      (List(uuid1), "List(" + uuid1.toString + ")", "uuids"),
      (List(uuid2), "List(" + uuid2.toString + ")", s"""List("$uuid2")"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uuids", "UUID", "string", 2), testData)
  }


  def uris(): Unit = {

    val testData = List(
      (Nil, "List()", ""),
      (Nil, "List()", "List()"),
      (List(uri1), "List(" + uri1.toString + ")", "uris"),
      (List(uri2), "List(" + uri2.toString + ")", s"""List("$uri2")"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uris", "URI", "string", 2), testData)
  }
}