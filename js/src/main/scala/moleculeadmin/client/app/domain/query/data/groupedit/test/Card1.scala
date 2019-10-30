package moleculeadmin.client.app.domain.query.data.groupedit.test

import java.time.LocalDateTime
import java.util.UUID
import moleculeadmin.client.app.domain.query.QueryState.columns
import moleculeadmin.client.app.domain.query.data.groupedit.ops.ScalaCode
import moleculeadmin.client.scalafiddle.ScalafiddleApi
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.util.HelpersAdmin
import scala.concurrent.ExecutionContext.Implicits.global


object Card1 extends HelpersAdmin {

  // Long treated as String for BigInt consumption
  // to avoid precision issues on js side
  val eid = "1"

  val eCol: Col = Col(0, 0, "Ns", "Ns", "e", "datom", "double", 1)

  case class Compiler(col: Col) {

    val opt = col.attr.last == '$'

    val attrStr = col.attr + " " * (15 - col.attr.length)

    columns() = List(
      eCol,
      col.copy(colIndex = 1, attrExpr = "orig"),
      col.copy(colIndex = 2, attrExpr = "edit")
    )

    val err = """__ERR__(.*)__ERR__(.)""".r

    def test[T](unitTest: (Option[T], String, String)*): Unit = {
      unitTest.foreach {
        case (baseValue, expected0, rhs) => {
          val expected = if (expected0 == "__None__") "None" else s"Some($expected0)"

          val scalaCode: String = ScalaCode(col, rhs).get
          //                println(scalaCode)

          ScalafiddleApi[String](scalaCode).lambda2.foreach { lambda =>

            val rawNevValue: String = lambda(eid, if (opt) baseValue else baseValue.get)

            val (errMsg0, newValue) = rawNevValue match {
              case "__None__"     => ("", "None")
              case err(errMsg, v) => (errMsg, s"Some($v)")
              case v              => ("", s"Some($v)")
            }

            val errMsg = if (errMsg0.nonEmpty) s"\nError          : $errMsg0" else ""

            val rhsStr = if (rhs.contains("\n")) s"\n$rhs" else rhs

            if (newValue == expected)
              println(
                s"""$attrStr: $baseValue
                   |rhs            : $rhsStr
                   |Expected result: $newValue$errMsg
                   |""".stripMargin)
            else {
              throw new IllegalArgumentException(
                s"""
                   |***************** UNEXPECTED RESULT *************************
                   |$attrStr: $baseValue
                   |rhs            : $rhsStr
                   |Expected result: $expected
                   |Actual result  : $newValue$errMsg
                   |----- Scala code: -----
                   |$scalaCode
                   |""".stripMargin
              )
            }
          }
        }
      }
    }
  }

  //  implicit def str2ldt2(s: String): LocalDateTime = {
  //    val nano = """(\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])\.(\d{1,9})""".r
  //    val sec  = """(\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])""".r
  //    val min  = """(\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])""".r
  //    val ymd  = """(\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])""".r
  //    try {
  //      s match {
  //        case nano(y, m, d, hh, mm, ss, n) => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, ss.toInt, n.padTo(9, '0').toInt)
  //        case sec(y, m, d, hh, mm, ss)     => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, ss.toInt, 0)
  //        case min(y, m, d, hh, mm)         => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, 0, 0)
  //        case ymd(y, m, d)                 => LocalDateTime.of(y.toInt, m.toInt, d.toInt, 0, 0, 0, 0)
  //        case other                        => throw new IllegalArgumentException("Unexpected date string: " + other)
  //      }
  //    } catch {
  //      case e: Throwable =>
  //        error = e.toString
  //        LocalDateTime.MIN
  //    }
  //  }


  def string(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "str", "String", "string", 1)
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some("a2"), "", """Some("")"""),
      (Some("a2"), " ", """Some(" ")"""),
      (Some("a2"), "\n ", """Some("\n ")"""),
      (Some("a2"), " \n ", """Some(" \n ")"""),
      (Some("a3"), "Code is:\na3", """Some("Code is:\n" + str)"""),
      (Some("a3"), "a31", "Some(str + 1)"),
      (Some("a3"), "aaa", """Some(str.init * 3)"""),
    )
  }

  def bool(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "bool", "Boolean", "string", 1, attrExpr = "edit")
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some(true), "false", "Some(false)"),
      (Some(true), "true", "Some(bool)"),
      (Some(true), "false", "Some(!bool)"),
      (Some(true), "false", "Some(bool && 7 == 8)"),
      (Some(true), "true", "Some(bool || 7 == 8)"),
    )
  }

  def date(): Unit = {
    val dummy = Some(LocalDateTime.MIN)
    val date  = Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11))
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

    // Mandatory
    Compiler(
      Col(2, 0, "Ns", "Ns", "date", "Date", "string", 1)
    ).test(
      (Some(LocalDateTime.of(2001, 3, 5, 0, 0)), "2001-03-05T00:00", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9)), "2001-03-05T07:09", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11)), "2001-03-05T07:09:11", "Some(date)"),

      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 100000000)), "2001-03-05T07:09:11.100", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 10000000)), "2001-03-05T07:09:11.010", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 1000000)), "2001-03-05T07:09:11.001", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 100000)), "2001-03-05T07:09:11.000100", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 10000)), "2001-03-05T07:09:11.000010", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 1000)), "2001-03-05T07:09:11.000001", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 100)), "2001-03-05T07:09:11.000000100", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 10)), "2001-03-05T07:09:11.000000010", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 1)), "2001-03-05T07:09:11.000000001", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11)), "2001-03-05T07:09:11", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 1)), "2001-03-05T07:09:01", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9)), "2001-03-05T07:09", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 0, 0)), "2001-03-05T00:00", "Some(date)"),

      (dummy, "2001-03-05T07:09:11.100", """Some("2001-3-5 7:9:11.1000")"""),
      (dummy, "2001-03-05T07:09:11.100", """Some("2001-3-5 7:9:11.100")"""),
      (dummy, "2001-03-05T07:09:11.100", """Some("2001-3-5 7:9:11.10")"""),
      (dummy, "2001-03-05T07:09:11.100", """Some("2001-3-5 7:9:11.1")"""),
      (dummy, "2001-03-05T07:09:11.010", """Some("2001-3-5 7:9:11.01")"""),
      (dummy, "2001-03-05T07:09:11.001", """Some("2001-3-5 7:9:11.001")"""),
      (dummy, "2001-03-05T07:09:11.000100", """Some("2001-3-5 7:9:11.0001")"""),
      (dummy, "2001-03-05T07:09:11.000010", """Some("2001-3-5 7:9:11.00001")"""),
      (dummy, "2001-03-05T07:09:11.000001", """Some("2001-3-5 7:9:11.000001")"""),
      (dummy, "2001-03-05T07:09:11.000000100", """Some("2001-3-5 7:9:11.0000001")"""),
      (dummy, "2001-03-05T07:09:11.000000010", """Some("2001-3-5 7:9:11.00000001")"""),
      (dummy, "2001-03-05T07:09:11.000000001", """Some("2001-3-5 7:9:11.000000001")"""),
      (dummy, "2001-03-05T07:09:11", """Some("2001-03-05 07:09:11.000")"""),
      (dummy, "2001-03-05T07:09:11", """Some("2001-3-5 7:9:11.00")"""),
      (dummy, "2001-03-05T07:09:11", """Some("2001-3-5 7:9:11.0")"""),
      (dummy, "2001-03-05T07:09:11", """Some("2001-3-5 7:9:11")"""),
      (dummy, "2001-03-05T07:09:10", """Some("2001-3-5 7:9:10")"""),
      (dummy, "2001-03-05T07:09:01", """Some("2001-3-5 7:9:01")"""),
      (dummy, "2001-03-05T07:09:01", """Some("2001-3-5 7:9:1")"""),
      (dummy, "2001-03-05T07:09", """Some("2001-3-5 7:9:00")"""),
      (dummy, "2001-03-05T07:09", """Some("2001-3-5 7:9:0")"""),
      (dummy, "2001-03-05T07:09", """Some("2001-3-5 7:9")"""),
      (dummy, "2001-03-05T07:00", """Some("2001-3-5 7:0")"""),
      (dummy, "2001-03-05T00:00", """Some("2001-3-5 0:0")"""),
      (dummy, "2001-03-05T00:00", """Some("2001-3-5")"""),

      (date, "2001-03-05T07:09:11", "Some(date)"),

      // Adjust backwards in time
      (date, "2000-03-05T07:09:11", "Some(date.minusYears(1))"),
      (date, "2001-02-05T07:09:11", "Some(date.minusMonths(1))"),
      (date, "2001-02-26T07:09:11", "Some(date.minusWeeks(1))"),
      (date, "2001-03-04T07:09:11", "Some(date.minusDays(1))"),
      (date, "2001-03-05T06:09:11", "Some(date.minusHours(1))"),
      (date, "2001-03-05T07:08:11", "Some(date.minusMinutes(1))"),
      (date, "2001-03-05T07:09:10", "Some(date.minusSeconds(1))"),
      (date, "2001-03-05T07:09:10.900", "Some(date.minusNanos(100 * 1000 * 1000))"),
      (Some(LocalDateTime.of(2002, 1, 1, 0, 0)), "2001-12-31T23:59:59", "Some(date.minusSeconds(1))"),

      // Set time
      (date, "2000-03-05T07:09:11", "Some(date.withYear(2000))"),
      (date, "2001-01-05T07:09:11", "Some(date.withMonth(1))"),
      (date, "2001-03-24T07:09:11", "Some(date.withDayOfMonth(24))"),
      (date, "2001-02-09T07:09:11", "Some(date.withDayOfYear(40))"),
      (date, "2001-03-05T01:09:11", "Some(date.withHour(1))"),
      (date, "2001-03-05T07:01:11", "Some(date.withMinute(1))"),
      (date, "2001-03-05T07:09:01", "Some(date.withSecond(1))"),

      // Adjust forward in time
      (date, "2002-03-05T07:09:11", "Some(date.plusYears(1))"),
      (date, "2001-04-05T07:09:11", "Some(date.plusMonths(1))"),
      (date, "2001-03-12T07:09:11", "Some(date.plusWeeks(1))"),
      (date, "2001-03-06T07:09:11", "Some(date.plusDays(1))"),
      (date, "2001-03-05T08:09:11", "Some(date.plusHours(1))"),
      (date, "2001-03-05T07:10:11", "Some(date.plusMinutes(1))"),
      (date, "2001-03-05T07:09:12", "Some(date.plusSeconds(1))"),
      (date, "2001-03-05T07:09:11.123", "Some(date.plusNanos(123 * 1000 * 1000))"),
      (Some(LocalDateTime.of(2001, 12, 31, 23, 59, 59)), "2002-01-01T00:00", "Some(date.plusSeconds(1))"),

      // Now
      // Might fail if executed on each side of hour/minute change
      (dummy, s"$y-$m-${d}T$hh:00", "Some(LocalDateTime.now().withMinute(0).withSecond(0).withNano(0))"),
      (dummy, s"$y-$m-${d}T$hh:$mm", "Some(LocalDateTime.now().withSecond(0).withNano(0))"),
    )

    // Optional
    Compiler(
      Col(2, 0, "Ns", "Ns", "date$", "Date", "string", 1, true)
    ).test(
      //      (None, "None", ""),
      //      (None, "None", "None"),
      //      (Some(LocalDateTime.of(2001, 3, 5, 0, 0)), "2001-03-05T00:00", "date$.fold(Option.empty[LocalDateTime])(v => Some(v))"),
      //      (Some(LocalDateTime.of(2001, 3, 5, 0, 0)), "2001-03-05T00:00", "date$.fold(None)(v => Some(v))"),
      (Some(LocalDateTime.of(2001, 3, 5, 0, 0)), "2001-03-05T00:00",
        """date$ match {
          |  case Some(v) => Some(v)
          |  case None    => None
          |}""".stripMargin
      ),
    )
  }

  def uuid(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "uuid", "UUID", "string", 1, attrExpr = "edit")
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some(""), "false", "Some(false)"),
      (Some(""), "true", "Some(bool)"),
      (Some(""), "false", "Some(!bool)"),
      (Some(""), "false", "Some(bool && 7 == 8)"),
      (Some(""), "true", "Some(bool || 7 == 8)"),
    )
  }

  def uri(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "uri", "URI", "string", 1, attrExpr = "edit")
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some(""), "false", "Some(false)"),
      (Some(""), "true", "Some(bool)"),
      (Some(""), "false", "Some(!bool)"),
      (Some(""), "false", "Some(bool && 7 == 8)"),
      (Some(""), "true", "Some(bool || 7 == 8)"),
    )
  }

  def int(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "int", "Int", "double", 1, attrExpr = "edit")
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some(1), "2", "Some(int + e)"),
      (Some(1), "3", "Some(int + 2)"),
      (Some(1), "4", "Some(int + BigInt(3))"),
      (Some(1), "5", "Some(5)"),
      (Some(1), "6",
        """int match {
          |   case n if n > 10 => Some(42)
          |   case n           => Some(n * 7 - 1)
          |}""".stripMargin),

      //      // Floats not allowed in Int expression
      //      // Doubles not allowed in Int expression
      //      // BigDecimals not allowed in Int expression
      (None, "-", "Some(4.2f)"),
      (None, "-", "Some(4.2)"),
      (None, "-", "Some(BigDecimal(4.2))"),
    )
  }

  // Same semantics as with 'datom' and 'ref' (both Long)
  def long(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "long", "Long", "double", 1, attrExpr = "edit")
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some("2"), "3", "Some(long + e)"),
      (Some("2"), "4", "Some(long + 2)"),
      (Some("2"), "5", "Some(5)"),
      (Some("2"), "6",
        """long match {
          |   case n if n > 10 => Some(42)
          |   case n           => Some(n * 3)
          |}""".stripMargin),

      //      // Floats not allowed in Long expression
      //      // Doubles not allowed in Long expression
      //      // BigDecimals not allowed in Long expression
      (None, "-", "Some(4.2f)"),
      (None, "-", "Some(4.2)"),
      (None, "-", "Some(BigDecimal(4.2))"),
    )
  }

  def float(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "float", "Float", "double", 1, attrExpr = "edit")
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some("3.1"), "4.1", "Some(float + e)"),
      (Some("3.1"), "5.1", "Some(float + 2L)"),
      (Some("3.1"), "6.1", "Some(float + 3)"),
      (Some("3.1"), "7.1", "Some(float + BigInt(4))"),
      (Some("3.1"), "8.2", "Some(float + 5.1)"),
      (Some("3.1"), "8.3", "Some(float + 5.2)"),
      (Some("3.1"), "8.4", "Some(float + BigDecimal(5.3))"),
      (Some("3.1"), "8.5", "Some(8.5)"),
      (Some("3.1"), "9.3",
        """float match {
          |   case n if n > 10 => Some(42)
          |   case n           => Some(n * 3)
          |}""".stripMargin),

      //      // Use Double instead of Floats
      (None, "-", "Some(4.2f)"),
    )
  }

  def double(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "double", "Double", "double", 1, attrExpr = "edit")
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some("4.1"), "5.1", "Some(double + e)"),
      (Some("4.1"), "6.1", "Some(double + 2L)"),
      (Some("4.1"), "7.1", "Some(double + 3)"),
      (Some("4.1"), "8.1", "Some(double + BigInt(4))"),
      (Some("4.1"), "9.2", "Some(double + 5.1)"),
      (Some("4.1"), "9.3", "Some(double + 5.2)"),
      (Some("4.1"), "9.4", "Some(double + BigDecimal(5.3))"),
      (Some("4.1"), "9.5", "Some(9.5)"),
      (Some("4.1"), "10.3",
        """double match {
          |   case n if n > 10 => Some(42)
          |   case n           => Some(n * 3 - 2)
          |}""".stripMargin),

      //      // Use Double instead of Floats
      (None, "-", "Some(4.2f)"),
    )
  }

  def bigInt(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "bigInt", "BigInt", "double", 1, attrExpr = "edit")
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some("5"), "6", "Some(bigInt + e)"),
      (Some("5"), "7", "Some(bigInt + 2)"),
      (Some("5"), "8", "Some(8)"),
      (Some("5"), "9",
        """bigInt match {
          |   case n if n > 10 => Some(42)
          |   case n           => Some(n * 2 - 1)
          |}""".stripMargin),

      //      // Floats not allowed
      //      // Doubles not allowed
      //      // BigDecimals not allowed in BigInt expression
      (None, "-", "Some(4.2f)"),
      (None, "-", "Some(4.2)"),
      (None, "-", "Some(BigDecimal(4.2))"),
    )
  }

  def bigDecimal(): Unit = {
    Compiler(
      Col(2, 0, "Ns", "Ns", "bigDec", "BigDecimal", "double", 1, attrExpr = "edit")
    ).test(
      (None, "None", ""),
      (None, "None", "None"),
      (Some("6.1"), "7.1", "Some(bigDec + e)"),
      (Some("6.1"), "8.1", "Some(bigDec + 2L)"),
      (Some("6.1"), "9.1", "Some(bigDec + 3)"),
      (Some("6.1"), "10.1", "Some(bigDec + BigInt(4))"),
      (Some("6.1"), "11.2", "Some(bigDec + 5.1)"),
      (Some("6.1"), "11.3", "Some(bigDec + 5.2)"),
      (Some("6.1"), "11.4", "Some(bigDec + BigDecimal(5.3))"),
      (Some("6.1"), "11.5", "Some(11.5)"),
      (Some("6.1"), "12.2",
        """bigDec match {
          |   case n if n > 10 => Some(42)
          |   case n           => Some(n * 2)
          |}""".stripMargin),

      //      // Use Double instead of Floats
      (None, "-", "Some(4.2f)"),
    )
  }
}