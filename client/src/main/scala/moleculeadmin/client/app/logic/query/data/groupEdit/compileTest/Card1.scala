package moleculeadmin.client.app.logic.query.data.groupEdit.compileTest

import java.time.LocalDateTime
import moleculeadmin.client.app.logic.query.QueryState.columns
import moleculeadmin.client.app.logic.query.data.groupEdit.ops.ScalaCode
import moleculeadmin.client.scalafiddle.ScalaFiddle
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.ColOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters._


/** Testing various group edit operations for cardinality-one attributes.
 *
 * MoleculeAdmin transforms each cardinality-one value in a group edit column
 * by applying a lambda expression that takes the current value as input and
 * transform the value.
 *
 * For cardinality-one attributes, the expected transformation lambda type
 * is `attr-type => Option[attr-type]`. If the right-hand side is `None`, the
 * value will be retracted.
 *
 * Since we always expect the left-hand side input to be the attribute value,
 * only the right-hand side of the lambda is entered in the input field of
 * the group edit column.
 *
 * So, for instance to increase the age for all persons,
 * one can simply write `Some(age + 1)` where "age" refers to a cardinality-one
 * attribute `age`.
 *
 * MoleculeAdmin will then apply the lambda `age => Some(age + 1)` to all
 * age values in the group edit column - both on the current and subsequent
 * pages! Therefore, make sure to filter only the values that you
 * want to transform!
 *
 * Below you can see various examples of right-hand side expressions for all
 * attribute types.
 */
object Card1 extends TestScalaFiddle with ColOps {

  case class Compiler(
    col: Col,
    testData: List[(Option[String], String, String)]
  ) extends BaseTestGroupEdit(col) {
    val mandatory = !optional
    val attrOpt   = if (optional) attr else attr + "$"

    def rhsIsOpt(rhs: String): Boolean = rhs.contains(attrOpt)

    val attrResolver = ResolveAttrs(columns.now)


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
              val attrClean  = attrResolver.postfixed(col)
              val cleanSpace = " " * (attrClean.length + 1)
              s"""$attr match {
                 |  case Some($attrClean) => $someRhs
                 |  case None$cleanSpace  => None
                 |}""".stripMargin
          }
        } else {
          rhs0
        }

        val scalaCode: String = ScalaCode(columns.now, col, rhs).get
        println(scalaCode)

        ScalaFiddle[js.UndefOr[String]](scalaCode).lambda2.foreach { lambda =>
          def process[T](input: T): (Option[String], String) = {
            lambda(eid, input) match {
              case js.Tuple2(v, err) =>
                if (err.nonEmpty)
                  (None, err)
                else if (v.asInstanceOf[js.UndefOr[_]].isEmpty)
                  (None, "")
                else
                  (v.toOption, "")
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
      // Only Int's accepted.
      //      (Some("1"), "None", "Some(2L)"),
      //      (Some("1"), "None", "Some(BigInt(2))"),
      //      (Some("1"), "None", "Some(1.1f)"),
      //      (Some("1"), "None", "Some(1.1)"),
      //      (Some("1"), "None", "Some(BigDecimal(1.1))"),

      (None, "None", ""),
      (None, "None", "None"),

      (Some("1"), "Some(2)", "Some(2)"),
      (Some("1"), "Some(2)", "Some(int + 1)"),

      // We can pattern match Int attr against Int value since Int's are not converted to BigInt
      (Some("1"), "Some(2)",
        """int match {
          |  case 1 => Some(2)
          |  case _ => None
          |}""".stripMargin),

      (Some("1"), "Some(3)",
        """int match {
          |  case v if v > 5 => Some(2)
          |  case v          => Some(v * 3)
          |}""".stripMargin),

      (Some("1"), "Some(2)",
        """int$ match {
          |  case Some(1) => Some(2)
          |  case _       => None
          |}""".stripMargin),

      (Some("1"), "Some(3)",
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
    Compiler(Col(2, 0, "Ns", "Ns", "int$", "Int", "double", 1, true), testData)
  }


  def long(): Unit = {
    val testData = List(
      // Floating point number types non-compatible
      //      (Some("1"), "Some(1.0)", "Some(1.0f)"),
      //      (Some("1"), "Some(1.0)", "Some(1.0)"),
      //      (Some("1"), "Some(1.0)", "Some(BigDecimal(1.0))"),

      (None, "None", ""),
      (None, "None", "None"),

      (Some("1"), "Some(2)", "Some(2)"),
      (Some("1"), "Some(2)", "Some(2L)"),
      (Some("1"), "Some(2)", "Some(BigInt(2))"),

      (Some("1"), "Some(2)", "Some(long + 1)"),
      (Some("1"), "Some(2)", "Some(long + 1L)"),
      (Some("1"), "Some(2)", "Some(long + e)"),
      (Some("1"), "Some(2)", "Some(long + BigInt(1))"),

      (Some("1"), "Some(2)", "Some(1 + long)"),
      (Some("1"), "Some(2)", "Some(1L + long)"),
      (Some("1"), "Some(2)", "Some(e + long)"),
      (Some("1"), "Some(2)", "Some(BigInt(1) + long)"),

      //      // Can't pattern match BigInt (`long` converted) with Int/Long value directly
      //      (Some("1"), "Some(2)",
      //        """long match {
      //          |  case 1 => Some(2L)
      //          |  case _ => None
      //          |}""".stripMargin),

      // Instead we can compare an implicitly converted value `v`
      (Some("1"), "Some(2)",
        """long match {
          |  case v if v == 1 => Some(v + 1)
          |  case _           => None
          |}""".stripMargin),

      //      // Can't pattern match BigInt (`long` converted) with Int/Long value directly
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
    Compiler(Col(2, 0, "Ns", "Ns", "long$", "Long", "double", 1, true), testData)
  }


  def bigInt(): Unit = {
    val testData = List(
      // Floating point number types non-compatible
      //      (Some("1"), "Some(1.0)", "Some(1.0f)"),
      //      (Some("1"), "Some(1.0)", "Some(1.0)"),
      //      (Some("1"), "Some(1.0)", "Some(BigDecimal(1.0))"),

      (None, "None", ""),
      (None, "None", "None"),

      (Some("1"), "Some(2)", "Some(2)"),
      (Some("1"), "Some(2)", "Some(2L)"),
      (Some("1"), "Some(2)", "Some(BigInt(2))"),

      (Some("1"), "Some(2)", "Some(bigInt + 1)"),
      (Some("1"), "Some(2)", "Some(bigInt + 1L)"),
      (Some("1"), "Some(2)", "Some(bigInt + BigInt(1))"),

      //      // Can't pattern match BigInt
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

      //      // Can't pattern match BigInt
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
    Compiler(Col(2, 0, "Ns", "Ns", "bigInt$", "BigInt", "double", 1, true), testData)
  }


  def float(): Unit = {
    val testData = List(
      // blacklist types
      (Some("1"), s"$iae Please use Double instead of Float in expression `Some(4.2f)` to get correct floating point precision.", "Some(4.2f)"),

      (None, "None", ""),
      (None, "None", "None"),

      (Some("1.1"), "Some(2)", "Some(2)"),
      (Some("1.1"), "Some(2)", "Some(2L)"),
      (Some("1.1"), "Some(2)", "Some(BigInt(2))"),
      (Some("1.1"), "Some(2.1)", "Some(2.1)"),
      (Some("1.1"), "Some(2.1)", "Some(BigDecimal(2.1))"),

      (Some("1.1"), "Some(2.1)", "Some(float + 1)"),
      (Some("1.1"), "Some(2.1)", "Some(float + 1L)"),
      (Some("1.1"), "Some(2.1)", "Some(float + BigInt(1))"),
      (Some("1.1"), "Some(2.3)", "Some(float + 1.2)"),
      (Some("1.1"), "Some(2.3)", "Some(float + BigDecimal(1.2))"),

      //      // Can't pattern match BigDecimal (`float` converted) with Float/Double value directly
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

      //      // Can't pattern match BigDecimal (`float` converted) with Float/Double value directly
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
    Compiler(Col(2, 0, "Ns", "Ns", "float$", "Float", "double", 1, true), testData)
  }


  def double(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),

      // blacklist types
      (Some("1"), s"$iae Please use Double instead of Float in expression `Some(4.2f)` to get correct floating point precision.", "Some(4.2f)"),

      // whitelist types
      (Some("1.1"), "Some(2)", "Some(2)"),
      (Some("1.1"), "Some(2)", "Some(2L)"),
      (Some("1.1"), "Some(2)", "Some(BigInt(2))"),
      (Some("1.1"), "Some(2.1)", "Some(2.1)"),
      (Some("1.1"), "Some(2.1)", "Some(BigDecimal(2.1))"),

      (Some("1.1"), "Some(2.1)", "Some(double + 1)"),
      (Some("1.1"), "Some(2.1)", "Some(double + 1L)"),
      (Some("1.1"), "Some(2.1)", "Some(double + BigInt(1))"),
      (Some("1.1"), "Some(2.3)", "Some(double + 1.2)"),
      (Some("1.1"), "Some(2.3)", "Some(double + BigDecimal(1.2))"),

      //      // Can't pattern match BigDecimal (`double` converted) with Float/Double value directly
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

      //      // Can't pattern match BigDecimal (`double` converted) with Float/Double value directly
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
    Compiler(Col(2, 0, "Ns", "Ns", "double$", "Double", "double", 1, true), testData)
  }


  def bigDec(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),

      // blacklist types
      (Some("1"), s"$iae Please use Double instead of Float in expression `Some(4.2f)` to get correct floating point precision.", "Some(4.2f)"),

      // whitelist types
      (Some("1.1"), "Some(2)", "Some(2)"),
      (Some("1.1"), "Some(2)", "Some(2L)"),
      (Some("1.1"), "Some(2)", "Some(BigInt(2))"),
      (Some("1.1"), "Some(2.1)", "Some(2.1)"),
      (Some("1.1"), "Some(2.1)", "Some(BigDecimal(2.1))"),

      (Some("1.1"), "Some(2.1)", "Some(bigDec + 1)"),
      (Some("1.1"), "Some(2.1)", "Some(bigDec + 1L)"),
      (Some("1.1"), "Some(2.1)", "Some(bigDec + BigInt(1))"),
      (Some("1.1"), "Some(2.3)", "Some(bigDec + 1.2)"),
      (Some("1.1"), "Some(2.3)", "Some(bigDec + BigDecimal(1.2))"),

      //      // Can't pattern match BigDecimal
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

      //      // Can't pattern match BigDecimal
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
    Compiler(Col(2, 0, "Ns", "Ns", "bigDec$", "BigDecimal", "double", 1, true), testData)
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
    Compiler(Col(2, 0, "Ns", "Ns", "str$", "String", "string", 1, true), testData)
  }


  def bool(): Unit = {
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),
      (Some("true"), "Some(false)", "Some(false)"),
      (Some("true"), "Some(true)", "Some(bool)"),
      (Some("true"), "Some(false)", "Some(!bool)"),
      (Some("true"), "Some(false)", "Some(bool && 7 == 8)"),
      (Some("true"), "Some(true)", "Some(bool || 7 == 8)"),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bool", "Boolean", "string", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "bool$", "Boolean", "string", 1, true), testData)
  }


  def date(): Unit = {
    val someDate = Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11).toString)
    val nowDate  = LocalDateTime.now()
    val now      = nowDate.toString
    def p(s: Int, i: Int = 2): String = i match {
      case 2 => "%02d".format(s)
      case 3 => "%03d".format(s)
      case 4 => "%04d".format(s)
    }
    def y: Int = nowDate.getYear
    def m: String = p(nowDate.getMonthValue)
    def d: String = p(nowDate.getDayOfMonth)
    def hh: String = p(nowDate.getHour)
    def mm: String = p(nowDate.getMinute)

    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),
      (None, "None", "date$"),

      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 100000000).toString), "Some(2001-03-05T07:09:11.100)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 10000000).toString), "Some(2001-03-05T07:09:11.010)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 1000000).toString), "Some(2001-03-05T07:09:11.001)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 0).toString), "Some(2001-03-05T07:09:11)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11).toString), "Some(2001-03-05T07:09:11)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11).toString), "Some(2001-03-05T07:09:11)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 1).toString), "Some(2001-03-05T07:09:01)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9, 0).toString), "Some(2001-03-05T07:09)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9).toString), "Some(2001-03-05T07:09)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 9).toString), "Some(2001-03-05T07:09)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 7, 0).toString), "Some(2001-03-05T07:00)", "Some(date)"),
      (Some(LocalDateTime.of(2001, 3, 5, 0, 0).toString), "Some(2001-03-05T00:00)", "Some(date)"),

      // Construct LocalDateTime with String
      (None, "Some(2001-03-05T07:09:11.100)", """Some("2001-3-5 7:9:11.1")"""),
      (None, "Some(2001-03-05T07:09:11.100)", """Some("2001-3-5 7:9:11.10")"""),
      (None, "Some(2001-03-05T07:09:11.100)", """Some("2001-3-5 7:9:11.100")"""),
      (None, "Some(2001-03-05T07:09:11.010)", """Some("2001-3-5 7:9:11.01")"""),
      (None, "Some(2001-03-05T07:09:11.010)", """Some("2001-3-5 7:9:11.010")"""),
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
      (None, "Some(2001-03-01T00:00)", """Some("2001-3")"""),
      (None, "Some(2001-01-01T00:00)", """Some("2001")"""),

      (None, s"$iae Please provide a non-empty date string", """Some("")"""),
      (None, s"$iae Non-valid date string: `2001.3.5`. " +
        s"Expected form for a full date is `2001-03-05 07:09:11.123`", """Some("2001.3.5")"""),

      // Construct LocalDateTime with `LocalDateTime.of(...)`
      (None, "Some(2001-03-05T07:09:11.123)", "Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 123 * 1000 * 1000))"),
      (None, "Some(2001-03-05T07:09:11)", "Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11))"),
      (None, "Some(2001-03-05T07:09)", "Some(LocalDateTime.of(2001, 3, 5, 7, 9))"),
      (None, "Some(2001-03-05T00:00)", "Some(LocalDateTime.of(2001, 3, 5, 0, 0))"),

      // Convenience LocalDateTime constructor
      (None, "Some(2001-03-05T07:09:11.123)", "Some(d(2001, 3, 5, 7, 9, 11, 123))"),
      (None, "Some(2001-03-05T07:09:11.012)", "Some(d(2001, 3, 5, 7, 9, 11, 12))"),
      (None, "Some(2001-03-05T07:09:11.001)", "Some(d(2001, 3, 5, 7, 9, 11, 1))"),
      (None, "Some(2001-03-05T07:09:11)", "Some(d(2001, 3, 5, 7, 9, 11))"),
      (None, "Some(2001-03-05T07:09)", "Some(d(2001, 3, 5, 7, 9))"),
      (None, "Some(2001-03-05T07:00)", "Some(d(2001, 3, 5, 7))"),
      (None, "Some(2001-03-05T00:00)", "Some(d(2001, 3, 5))"),
      (None, "Some(2001-03-01T00:00)", "Some(d(2001, 3))"),
      (None, "Some(2001-01-01T00:00)", "Some(d(2001))"),

      // LocalDateTime's are stored in Datomic as Date's having only
      // millisecond precision, so only ms precision is preserved:
      (None, "Some(2001-03-05T07:09:11.123)", "Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 123456789))"),
      (None, "Some(2001-03-05T07:09:11.012)", "Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 12345678))"),
      (None, "Some(2001-03-05T07:09:11.001)", "Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 1234567))"),
      (None, "Some(2001-03-05T07:09:11)", "Some(LocalDateTime.of(2001, 3, 5, 7, 9, 11, 123456))"),

      (None, s"$iae Milliseconds should be in range 0-999. Found: -1", "Some(d(2001, 3, 5, 7, 9, 11, -1))"),
      (None, s"$iae Milliseconds should be in range 0-999. Found: 2000", "Some(d(2001, 3, 5, 7, 9, 11, 2000))"),

      // Adjust backwards in time
      (someDate, "Some(2000-03-05T07:09:11)", "Some(date.minusYears(1))"),
      (someDate, "Some(2001-02-05T07:09:11)", "Some(date.minusMonths(1))"),
      (someDate, "Some(2001-02-26T07:09:11)", "Some(date.minusWeeks(1))"),
      (someDate, "Some(2001-03-04T07:09:11)", "Some(date.minusDays(1))"),
      (someDate, "Some(2001-03-05T06:09:11)", "Some(date.minusHours(1))"),
      (someDate, "Some(2001-03-05T07:08:11)", "Some(date.minusMinutes(1))"),
      (someDate, "Some(2001-03-05T07:09:10)", "Some(date.minusSeconds(1))"),
      (someDate, "Some(2001-03-05T07:09:10.900)", "Some(date.minusNanos(100 * 1000 * 1000))"),
      (Some(LocalDateTime.of(2002, 1, 1, 0, 0).toString), "Some(2001-12-31T23:59:59)", "Some(date.minusSeconds(1))"),

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
      (Some(LocalDateTime.of(2001, 12, 31, 23, 59, 59).toString), "Some(2002-01-01T00:00)", "Some(date.plusSeconds(1))"),

      // Now
      // Might fail if executed on each side of hour/minute change
      (Some(now), s"Some($y-$m-${d}T$hh:00)", "Some(LocalDateTime.now().withMinute(0).withSecond(0).withNano(0))"),
      (Some(now), s"Some($y-$m-${d}T$hh:$mm)", "Some(LocalDateTime.now().withSecond(0).withNano(0))"),
    )

    Compiler(Col(2, 0, "Ns", "Ns", "date", "Date", "string", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "date$", "Date", "string", 1, true), testData)
  }


  def uuid(): Unit = {
    val uuid1    = "aba20f8e-8e79-475c-b8d1-df11f57b29ba"
    val uuid2    = "b0924388-9c1b-4ce4-92ac-7b8d2b01beec"
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),
      (Some(uuid1), s"Some($uuid1)", "Some(uuid)"),
      (Some(uuid2), s"Some($uuid2)", s"""Some("$uuid2")"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uuid", "UUID", "string", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "uuid$", "UUID", "string", 1, true), testData)
  }


  def uri(): Unit = {
    val uri1     = "uri1"
    val uri2     = "uri2"
    val testData = List(
      (None, "None", ""),
      (None, "None", "None"),
      (Some(uri1), "Some(" + uri1.toString + ")", "Some(uri)"),
      (Some(uri2), "Some(" + uri2.toString + ")", s"""Some("$uri2")"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uri", "URI", "string", 1), testData)
    Compiler(Col(2, 0, "Ns", "Ns", "uri$", "URI", "string", 1, true), testData)
  }
}