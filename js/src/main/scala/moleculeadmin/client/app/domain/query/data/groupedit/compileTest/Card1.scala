package moleculeadmin.client.app.domain.query.data.groupedit.compileTest

import java.time.LocalDateTime
import java.util.UUID
import moleculeadmin.client.app.domain.query.data.groupedit.ops.ScalaCode
import moleculeadmin.client.scalafiddle.ScalaFiddle
import moleculeadmin.shared.ast.query.Col
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters._


object Card1 extends TestScalaFiddle {

  case class Compiler[AttrTransferType](
    col: Col,
    testData: List[(Option[AttrTransferType], String, String)]
  ) extends BaseTestGroupEdit(col) {
    val mandatory = !optional
    val attrOpt   = if (optional) attr else attr + "$"
    def rhsIsOpt(rhs: String): Boolean = rhs.contains(attrOpt)

    testData.foreach {
      case (None, _, _) if mandatory =>
        // mandatory values can't be None, so don't test
        println(s"Skipped mandatory attr $attr that can't have a None value")

      case (_, _, rhs) if mandatory && rhsIsOpt(rhs) =>
        // don't test optional expression on mandatory values
        println(s"Skipped mandatory attr $attr with option rhs: " + rhs)

      case (transferValue, expected, rhs0) => {
        val rhs = if (optional) {
          rhs0.trim match {
            case ""                              => ""
            case "None"                          => "None"
            case optRhs if rhsIsOpt(optRhs)      => optRhs
            case optRhs if transferValue.isEmpty => optRhs
            case someRhs                         =>
              val attrClean  = attr.init
              val cleanSpace = " " * (attrClean.length + 1)
              s"""$attr match {
                 |  case Some($attrClean) => $someRhs
                 |  case None$cleanSpace  => None
                 |}""".stripMargin
          }
        } else {
          rhs0
        }

        val scalaCode: String = ScalaCode(col, rhs).get
        //        println(scalaCode)

        ScalaFiddle[js.UndefOr[AttrTransferType]](scalaCode).lambda2.foreach { lambda =>
          def process[T](input: T): (Option[AttrTransferType], String) = {
            lambda(eid, input) match {
              case js.Tuple2(v, "") if v.isEmpty => (None, "")
              case js.Tuple2(v, "")              => (v.toOption, "")
              case js.Tuple2(_, error)           => (None, error)
            }
          }

          val (newVopt, error) = {
            if (optional) {
              process(transferValue.orUndefined)
            } else {
              process(transferValue.get)
            }
          }

          val input = if (optional) transferValue else transferValue.get

          showResult(rhs, input.toString, newVopt.toString, expected, error, scalaCode)
        }
      }
    }
  }


  def int(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),

      // blacklist types
      (Some(1), s"$iae Float not allowed in Int expression `Some(4.2f)`", "Some(4.2f)"),
      (Some(1), s"$iae Double not allowed in Int expression `Some(4.2)`", "Some(4.2)"),
      (Some(1), s"$iae BigDecimal not allowed in Int expression `Some(BigDecimal(4.2)`)", "Some(BigDecimal(4.2))"),

      // whitelist types
      (Some(1), "Some(1)", "Some(1)"),
      (Some(1), "Some(1)", "Some(1L)"),
      (Some(1), "Some(1)", "Some(BigInt(1))"),

      (Some(1), "Some(2)", "Some(int + 1)"),
      (Some(1), "Some(2)", "Some(int + 1L)"),
      (Some(1), "Some(2)", "Some(int + BigInt(1))"),

      // We can pattern match Int attr against Int value since Int's are not converted to BigInt
      (Some(1), "Some(2)",
        """int match {
          |  case 1 => Some(2)
          |  case _ => None
          |}""".stripMargin),

      (Some(1), "Some(3)",
        """int match {
          |  case v if v > 5 => Some(2)
          |  case v          => Some(v * 3)
          |}""".stripMargin),

      (Some(1), "Some(2)",
        """int$ match {
          |  case Some(1) => Some(2)
          |  case _       => None
          |}""".stripMargin),

      (Some(1), "Some(3)",
        """int$ match {
          |  case None    => Some(2)
          |  case Some(v) => Some(v + 2)
          |}""".stripMargin),

      (None, "Some(4)",
        """int$ match {
          |  case Some(v) => Some(v + 1)
          |  case None    => Some(4)
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "int", "Int", "double", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "int$", "Int", "double", 1), testData)
  }


  def long(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),

      // blacklist types
      (Some("1"), s"$iae Float not allowed in Long expression `Some(4.2f)`", "Some(4.2f)"),
      (Some("1"), s"$iae Double not allowed in Long expression `Some(4.2)`", "Some(4.2)"),
      (Some("1"), s"$iae BigDecimal not allowed in Long expression `Some(BigDecimal(4.2)`)", "Some(BigDecimal(4.2))"),

      // whitelist types
      (Some("1"), "Some(1)", "Some(1)"),
      (Some("1"), "Some(1)", "Some(1L)"),
      (Some("1"), "Some(1)", "Some(BigInt(1))"),

      (Some("1"), "Some(2)", "Some(long + 1)"),
      (Some("1"), "Some(2)", "Some(long + 1L)"),
      (Some("1"), "Some(2)", "Some(long + BigInt(1))"),

      //      // Can't pattern match BigInt (`long` converted) with Int value directly
      //      (Some("1"), "Some(2)",
      //        """long match {
      //          |  case 1 => Some(2)
      //          |  case _ => None
      //          |}""".stripMargin),

      // Instead we can compare an implicitly converted value `v`
      (Some("1"), "Some(2)",
        """long match {
          |  case v if v == 1 => Some(v + 1)
          |  case _           => None
          |}""".stripMargin),

      //      // Can't pattern match BigInt (`long` converted) with Int value directly
      //      (Some("1"), "Some(2)",
      //        """long$ match {
      //          |  case Some(1) => Some(2)
      //          |  case _       => None
      //          |}""".stripMargin),

      (Some("1"), "Some(2)",
        """long$ match {
          |  case Some(v) if v == 1 => Some(v + 1)
          |  case _                 => None
          |}""".stripMargin),

      (Some("1"), "Some(3)",
        """long$ match {
          |  case None    => Some(2)
          |  case Some(v) => Some(v + 2)
          |}""".stripMargin),

      (None, "Some(4)",
        """long$ match {
          |  case Some(v) => Some(v + 1)
          |  case None    => Some(4)
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "long", "Long", "double", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "long$", "Long", "double", 1), testData)
  }


  def bigInt(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),

      // blacklist types
      (Some("1"), s"$iae Float not allowed in BigInt expression `Some(4.2f)`", "Some(4.2f)"),
      (Some("1"), s"$iae Double not allowed in BigInt expression `Some(4.2)`", "Some(4.2)"),
      (Some("1"), s"$iae BigDecimal not allowed in BigInt expression `Some(BigDecimal(4.2)`)", "Some(BigDecimal(4.2))"),

      // whitelist types
      (Some("1"), "Some(1)", "Some(1)"),
      (Some("1"), "Some(1)", "Some(1L)"),
      (Some("1"), "Some(1)", "Some(BigInt(1))"),

      (Some("1"), "Some(2)", "Some(bigInt + 1)"),
      (Some("1"), "Some(2)", "Some(bigInt + 1L)"),
      (Some("1"), "Some(2)", "Some(bigInt + BigInt(1))"),

      //      // Can't pattern match BigInt (`int` converted) with Int value directly
      //      (Some("1"), "Some(2)",
      //        """bigInt match {
      //          |  case 1 => Some(2)
      //          |  case _ => None
      //          |}""".stripMargin),

      // Instead we can compare an implicitly converted value `v`
      (Some("1"), "Some(2)",
        """bigInt match {
          |  case v if v == 1 => Some(2)
          |  case _           => None
          |}""".stripMargin),

      //      // Can't pattern match BigInt (`int` converted) with Int value directly
      //      (Some("1"), "Some(2)",
      //        """bigInt$ match {
      //          |  case Some(1) => Some(2)
      //          |  case _       => None
      //          |}""".stripMargin),

      (Some("1"), "Some(2)",
        """bigInt$ match {
          |  case Some(v) if v == 1 => Some(v + 1)
          |  case _                 => None
          |}""".stripMargin),

      (Some("1"), "Some(3)",
        """bigInt$ match {
          |  case None    => Some(2)
          |  case Some(v) => Some(v + 2)
          |}""".stripMargin),

      (None, "Some(4)",
        """bigInt$ match {
          |  case Some(v) => Some(v + 1)
          |  case None    => Some(4)
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bigInt", "BigInt", "double", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "bigInt$", "BigInt", "double", 1), testData)
  }


  def float(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),

      // blacklist types
      (Some("1"), s"$iae Please use Double instead of Float in expression `Some(4.2f)` to get correct floating point precision.", "Some(4.2f)"),

      // whitelist types
      (Some("1.1"), "Some(1)", "Some(1)"),
      (Some("1.1"), "Some(1)", "Some(1L)"),
      (Some("1.1"), "Some(1)", "Some(BigInt(1))"),
      (Some("1.1"), "Some(1.2)", "Some(1.2)"),
      (Some("1.1"), "Some(1.2)", "Some(BigDecimal(1.2))"),

      (Some("1.1"), "Some(2.1)", "Some(float + 1)"),
      (Some("1.1"), "Some(2.1)", "Some(float + 1L)"),
      (Some("1.1"), "Some(2.1)", "Some(float + BigInt(1))"),
      (Some("1.1"), "Some(2.3)", "Some(float + 1.2)"),
      (Some("1.1"), "Some(2.3)", "Some(float + BigDecimal(1.2))"),

      //      // Can't pattern match BigInt (`float` converted) with Double value directly
      //      (Some("1.1"), "Some(2.3)",
      //        """float match {
      //          |  case 1.1 => Some(2.3)
      //          |  case _   => None
      //          |}""".stripMargin),

      // Instead we can compare an implicitly converted value `v`
      (Some("1.1"), "Some(2.3)",
        """float match {
          |  case v if v == 1.1 => Some(v + 1.2)
          |  case _             => None
          |}""".stripMargin),

      //      // Can't pattern match BigInt (`float` converted) with Double value directly
      //      (Some("1.1"), "Some(2.3)",
      //        """float$ match {
      //          |  case Some(1.1) => Some(2.3)
      //          |  case _         => None
      //          |}""".stripMargin),

      (Some("1.1"), "Some(2.3)",
        """float$ match {
          |  case Some(v) if v == 1.1 => Some(v + 1.2)
          |  case _                   => None
          |}""".stripMargin),

      (Some("1.1"), "Some(2.3)",
        """float$ match {
          |  case None    => Some(1.2)
          |  case Some(v) => Some(v + 1.2)
          |}""".stripMargin),

      (None, "Some(4.4)",
        """float$ match {
          |  case Some(v) => Some(v + 1.2)
          |  case None    => Some(4.4)
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "float", "Float", "double", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "float$", "Float", "double", 1), testData)
  }


  def double(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),

      // blacklist types
      (Some("1"), s"$iae Please use Double instead of Float in expression `Some(4.2f)` to get correct floating point precision.", "Some(4.2f)"),

      // whitelist types
      (Some("1.1"), "Some(1)", "Some(1)"),
      (Some("1.1"), "Some(1)", "Some(1L)"),
      (Some("1.1"), "Some(1)", "Some(BigInt(1))"),
      (Some("1.1"), "Some(1.2)", "Some(1.2)"),
      (Some("1.1"), "Some(1.2)", "Some(BigDecimal(1.2))"),

      (Some("1.1"), "Some(2.1)", "Some(double + 1)"),
      (Some("1.1"), "Some(2.1)", "Some(double + 1L)"),
      (Some("1.1"), "Some(2.1)", "Some(double + BigInt(1))"),
      (Some("1.1"), "Some(2.3)", "Some(double + 1.2)"),
      (Some("1.1"), "Some(2.3)", "Some(double + BigDecimal(1.2))"),

      //      // Can't pattern match BigInt (`double` converted) with Double value directly
      //      (Some("1.1"), "Some(2.3)",
      //        """double match {
      //          |  case 1.1 => Some(2.3)
      //          |  case _   => None
      //          |}""".stripMargin),

      // Instead we can compare an implicitly converted value `v`
      (Some("1.1"), "Some(2.3)",
        """double match {
          |  case v if v == 1.1 => Some(v + 1.2)
          |  case _             => None
          |}""".stripMargin),

      //      // Can't pattern match BigInt (`double` converted) with Double value directly
      //      (Some("1.1"), "Some(2.3)",
      //        """double$ match {
      //          |  case Some(1.1) => Some(2.3)
      //          |  case _         => None
      //          |}""".stripMargin),

      (Some("1.1"), "Some(2.3)",
        """double$ match {
          |  case Some(v) if v == 1.1 => Some(v + 1.2)
          |  case _                   => None
          |}""".stripMargin),

      (Some("1.1"), "Some(2.3)",
        """double$ match {
          |  case None    => Some(1.2)
          |  case Some(v) => Some(v + 1.2)
          |}""".stripMargin),

      (None, "Some(4.4)",
        """double$ match {
          |  case Some(v) => Some(v + 1.2)
          |  case None    => Some(4.4)
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "double", "Double", "double", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "double$", "Double", "double", 1), testData)
  }


  def bigDecimal(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),

      // blacklist types
      (Some("1"), s"$iae Please use Double instead of Float in expression `Some(4.2f)` to get correct floating point precision.", "Some(4.2f)"),

      // whitelist types
      (Some("1.1"), "Some(1)", "Some(1)"),
      (Some("1.1"), "Some(1)", "Some(1L)"),
      (Some("1.1"), "Some(1)", "Some(BigInt(1))"),
      (Some("1.1"), "Some(1.2)", "Some(1.2)"),
      (Some("1.1"), "Some(1.2)", "Some(BigDecimal(1.2))"),

      (Some("1.1"), "Some(2.1)", "Some(bigDec + 1)"),
      (Some("1.1"), "Some(2.1)", "Some(bigDec + 1L)"),
      (Some("1.1"), "Some(2.1)", "Some(bigDec + BigInt(1))"),
      (Some("1.1"), "Some(2.3)", "Some(bigDec + 1.2)"),
      (Some("1.1"), "Some(2.3)", "Some(bigDec + BigDecimal(1.2))"),

      //      // Can't pattern match BigInt (`bigDec` converted) with Double value directly
      //      (Some("1.1"), "Some(2.3)",
      //        """bigDec match {
      //          |  case 1.1 => Some(2.3)
      //          |  case _   => None
      //          |}""".stripMargin),

      // Instead we can compare an implicitly converted value `v`
      (Some("1.1"), "Some(2.3)",
        """bigDec match {
          |  case v if v == 1.1 => Some(v + 1.2)
          |  case _             => None
          |}""".stripMargin),

      //      // Can't pattern match BigInt (`bigDec` converted) with Double value directly
      //      (Some("1.1"), "Some(2.3)",
      //        """bigDec$ match {
      //          |  case Some(1.1) => Some(2.3)
      //          |  case _         => None
      //          |}""".stripMargin),

      (Some("1.1"), "Some(2.3)",
        """bigDec$ match {
          |  case Some(v) if v == 1.1 => Some(v + 1.2)
          |  case _                   => None
          |}""".stripMargin),

      (Some("1.1"), "Some(2.3)",
        """bigDec$ match {
          |  case None    => Some(1.2)
          |  case Some(v) => Some(v + 1.2)
          |}""".stripMargin),

      (None, "Some(4.4)",
        """bigDec$ match {
          |  case Some(v) => Some(v + 1.2)
          |  case None    => Some(4.4)
          |}""".stripMargin),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bigDec", "BigDecimal", "double", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "bigDec$", "BigDecimal", "double", 1), testData)
  }


  def string(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),
      (None, "Some(a)", """Some("a")"""),

      (Some("a"), "Some(b)",
        """str match {
          |  case "a" => Some("b")
          |  case _   => None
          |}""".stripMargin),

      (Some("a"), "Some(ab)",
        """str$ match {
          |  case Some(str) => Some(str + "b")
          |  case None      => None
          |}""".stripMargin),

      (Some("a"), "Some(b)",
        """str$ match {
          |  case Some("a") => Some("b")
          |  case _         => None
          |}""".stripMargin),

      (Some("a"), "None",
        """str$ match {
          |  case Some("x") => Some("b")
          |  case _         => None
          |}""".stripMargin),

      (None, "Some(none)",
        """str$ match {
          |  case Some(str) => Some(str + "b")
          |  case None      => Some("none")
          |}""".stripMargin),

      (Some("a2"), "Some(a2)", """Some(str)"""),
      (Some("a2"), "Some()", """Some("")"""),
      (Some("a2"), "Some( )", """Some(" ")"""),
      (Some("a2"), "Some(\n )", """Some("\n ")"""),
      (Some("a2"), "Some( \n )", """Some(" \n ")"""),
      (Some("a3"), "Some(Code is:\na3)", """Some("Code is:\n" + str)"""),
      (Some("a3"), "Some(a31)", "Some(str + 1)"),
      (Some("a3"), "Some(aaa)", """Some(str.init * 3)"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "str", "String", "string", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "str$", "String", "string", 1), testData)
  }


  def bool(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),
      (Some(true), "Some(false)", "Some(false)"),
      (Some(true), "Some(true)", "Some(bool)"),
      (Some(true), "Some(false)", "Some(!bool)"),
      (Some(true), "Some(false)", "Some(bool && 7 == 8)"),
      (Some(true), "Some(true)", "Some(bool || 7 == 8)"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bool", "Boolean", "string", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "bool$", "Boolean", "string", 1), testData)
  }


  def date(): Unit = {
    val someDate = Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11))
    val now      = LocalDateTime.now()
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
      (None, "None", ""),
      (None, "None", "None"),
      (None, "None", "date$"),
      (None, "Some(2001-03-05T00:00)", """Some("2001-03-05")"""),

      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 100000000)), "Some(2001-03-05T07:09:11.100)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 10000000)), "Some(2001-03-05T07:09:11.010)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 1000000)), "Some(2001-03-05T07:09:11.001)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 0)), "Some(2001-03-05T07:09:11)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11)), "Some(2001-03-05T07:09:11)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11)), "Some(2001-03-05T07:09:11)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 1)), "Some(2001-03-05T07:09:01)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 0)), "Some(2001-03-05T07:09)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9)), "Some(2001-03-05T07:09)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9)), "Some(2001-03-05T07:09)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 0)), "Some(2001-03-05T07:00)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 0, 0)), "Some(2001-03-05T00:00)", "Some(date)"),

      (None, "Some(2001-03-05T07:09:11.100)", """Some("2001-3-5 7:9:11.1")"""),
      (None, "Some(2001-03-05T07:09:11.010)", """Some("2001-3-5 7:9:11.01")"""),
      (None, "Some(2001-03-05T07:09:11.001)", """Some("2001-3-5 7:9:11.001")"""),
      (None, "Some(2001-03-05T07:09:11)", """Some("2001-03-05 07:09:11.000")"""),
      (None, "Some(2001-03-05T07:09:11)", """Some("2001-3-5 7:9:11.00")"""),
      (None, "Some(2001-03-05T07:09:11)", """Some("2001-3-5 7:9:11.0")"""),
      (None, "Some(2001-03-05T07:09:11)", """Some("2001-3-5 7:9:11")"""),
      (None, "Some(2001-03-05T07:09:10)", """Some("2001-3-5 7:9:10")"""),
      (None, "Some(2001-03-05T07:09:01)", """Some("2001-3-5 7:9:01")"""),
      (None, "Some(2001-03-05T07:09:01)", """Some("2001-3-5 7:9:1")"""),
      (None, "Some(2001-03-05T07:09)", """Some("2001-3-5 7:9:00")"""),
      (None, "Some(2001-03-05T07:09)", """Some("2001-3-5 7:9:0")"""),
      (None, "Some(2001-03-05T07:09)", """Some("2001-3-5 7:9")"""),
      (None, "Some(2001-03-05T07:00)", """Some("2001-3-5 7:0")"""),
      (None, "Some(2001-03-05T00:00)", """Some("2001-3-5 0:0")"""),
      (None, "Some(2001-03-05T00:00)", """Some("2001-3-5")"""),

      (someDate, "Some(2001-03-05T07:09:11)", "Some(date)"),

      // Adjust backwards in time
      (someDate, "Some(2000-03-05T07:09:11)", "Some(date.minusYears(1))"),
      (someDate, "Some(2001-02-05T07:09:11)", "Some(date.minusMonths(1))"),
      (someDate, "Some(2001-02-26T07:09:11)", "Some(date.minusWeeks(1))"),
      (someDate, "Some(2001-03-04T07:09:11)", "Some(date.minusDays(1))"),
      (someDate, "Some(2001-03-05T06:09:11)", "Some(date.minusHours(1))"),
      (someDate, "Some(2001-03-05T07:08:11)", "Some(date.minusMinutes(1))"),
      (someDate, "Some(2001-03-05T07:09:10)", "Some(date.minusSeconds(1))"),
      (someDate, "Some(2001-03-05T07:09:10.900)", "Some(date.minusNanos(100 * 1000 * 1000))"),
      (Some(LocalDateTime.of(2002, 1, 1, 0, 0)), "Some(2001-12-31T23:59:59)", "Some(date.minusSeconds(1))"),

      // Set time
      (someDate, "Some(2000-03-05T07:09:11)", "Some(date.withYear(2000))"),
      (someDate, "Some(2001-01-05T07:09:11)", "Some(date.withMonth(1))"),
      (someDate, "Some(2001-03-24T07:09:11)", "Some(date.withDayOfMonth(24))"),
      (someDate, "Some(2001-02-09T07:09:11)", "Some(date.withDayOfYear(40))"),
      (someDate, "Some(2001-03-05T01:09:11)", "Some(date.withHour(1))"),
      (someDate, "Some(2001-03-05T07:01:11)", "Some(date.withMinute(1))"),
      (someDate, "Some(2001-03-05T07:09:01)", "Some(date.withSecond(1))"),

      // Adjust forward in time
      (someDate, "Some(2002-03-05T07:09:11)", "Some(date.plusYears(1))"),
      (someDate, "Some(2001-04-05T07:09:11)", "Some(date.plusMonths(1))"),
      (someDate, "Some(2001-03-12T07:09:11)", "Some(date.plusWeeks(1))"),
      (someDate, "Some(2001-03-06T07:09:11)", "Some(date.plusDays(1))"),
      (someDate, "Some(2001-03-05T08:09:11)", "Some(date.plusHours(1))"),
      (someDate, "Some(2001-03-05T07:10:11)", "Some(date.plusMinutes(1))"),
      (someDate, "Some(2001-03-05T07:09:12)", "Some(date.plusSeconds(1))"),
      (someDate, "Some(2001-03-05T07:09:11.123)", "Some(date.plusNanos(123 * 1000 * 1000))"),
      (Some(LocalDateTime.of(2001, 12, 31, 23, 59, 59)), "Some(2002-01-01T00:00)", "Some(date.plusSeconds(1))"),

      // Now
      // Might fail if executed on each side of hour/minute change
      (Some(now), s"Some($y-$m-${d}T$hh:00)", "Some(LocalDateTime.now().withMinute(0).withSecond(0).withNano(0))"),
      (Some(now), s"Some($y-$m-${d}T$hh:$mm)", "Some(LocalDateTime.now().withSecond(0).withNano(0))"),

      (Some(LocalDateTime.of(2001, 7, 1, 0, 0)), "Some(" + date2str(date1) + "T00:00)", "Some(date)"),
    )

    Compiler(Col(2, 0, "Ns", "Ns", "date", "Date", "string", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "date$", "Date", "string", 1), testData)
  }


  def uuid(): Unit = {
    // Use stable uuid's to allow compiler to cache
    val uuid1    = UUID.fromString("aba20f8e-8e79-475c-b8d1-df11f57b29ba")
    val uuid2    = UUID.fromString("b0924388-9c1b-4ce4-92ac-7b8d2b01beec")
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),
      (Some(uuid1), "Some(" + uuid1.toString + ")", "Some(uuid)"),
      (Some(uuid2), "Some(" + uuid2.toString + ")", s"""Some("$uuid2")"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uuid", "UUID", "string", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "uuid$", "UUID", "string", 1), testData)
  }


  def uri(): Unit = {

    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),
      (Some(uri1), "Some(" + uri1.toString + ")", "Some(uri)"),
      (Some(uri2), "Some(" + uri2.toString + ")", s"""Some("$uri2")"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uri", "URI", "string", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "uri$", "URI", "string", 1), testData)
  }
}