package moleculeadmin.client.app.logic.query.data.groupEdit.compileTest

import java.time.LocalDateTime
import moleculeadmin.client.app.logic.query.QueryState.columns
import moleculeadmin.client.app.logic.query.data.groupEdit.ops.ScalaCode
import moleculeadmin.client.scalafiddle.ScalaFiddle
import moleculeadmin.shared.ast.query.Col
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters._


object Card3 extends TestScalaFiddle {

  val emptyMap = Map.empty[String, String]

  case class Compiler(
    col: Col,
    testData: List[(Map[String, String], String, String)]
  ) extends BaseTestGroupEdit(col) {

    val (attrOpt, attrClean) = if (optional)
      (attr, attr.init) else (attr + "$", attr)

    testData.foreach {
      case (attrValues, expected, rhs0) => {

        // Use optional notation with '$' in all tested rhs
        val rhs = rhs0.replace(attrOpt, attrClean)

        val scalaCode: String = ScalaCode(columns.now, col, rhs).get
        //        println(scalaCode)

        ScalaFiddle[js.Dictionary[String]](scalaCode).lambda2.foreach { lambda =>
          val (newValues, error) = lambda(eid, attrValues.toJSDictionary) match {
            case js.Tuple2(pairs, "") if pairs.isEmpty => (emptyMap, "")
            case js.Tuple2(pairs, "")                  => (pairs.toList, "")
            case js.Tuple2(_, error)                   => (emptyMap, error)
          }
          val newValuesMapStr    = newValues.map { case (k, v) => s"$k -> $v" }.mkString("Map(", ", ", ")")
          showResult(rhs, attrValues.toString, newValuesMapStr, expected, error, scalaCode)
        }
      }
    }
  }

  // No need to check both mandatory and optional attr values since they are both
  // conflated to a list that can be empty or contain values.

  def intMap(): Unit = {

    val testData = List(
      // Only Int's accepted.
      //      (emptyMap, "Map()", s"Map($a -> 2L)"),
      //      (emptyMap, "Map()", s"Map($a -> BigInt(2))"),
      //      (emptyMap, "Map()", s"Map($a -> 1.1f)"),
      //      (emptyMap, "Map()", s"Map($a -> 1.1)"),
      //      (emptyMap, "Map()", s"Map($a -> BigDecimal(1.1))"),

      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Nil"),
      (emptyMap, "Map()", "Map()"),
      (emptyMap, "Map()", "intMap"),

      (emptyMap, "Map(a -> 1)", """Map("a" -> 1)"""),
      (Map("a" -> "1"), "Map(a -> 1)", "intMap"),

      (Map("a" -> "1"), "Map(a -> 2)", """Map(intMap.head._1 -> 2)"""),
      (Map("a" -> "1"), "Map(a -> 2)", """Map(intMap.head._1 -> (intMap.head._2 + 1))"""),

      (Map("a" -> "1"), "Map(a -> 1, b -> 2)", """intMap + ("b" -> 2)"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "intMap", "Int", "mapDouble", 3), testData)
  }


  def longMap(): Unit = {
    val testData = List(
      // Floating point number types non-compatible
      //      (emptyMap, "Map(a -> 1.1)", """(Map("a" -> 1.1f)"""),
      //      (emptyMap, "Map(a -> 1.1)", """(Map("a" -> 1.1)"""),
      //      (emptyMap, "Map(a -> 1.1)", """(Map("a" -> BigDecimal(1.1))"""),

      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Nil"),
      (emptyMap, "Map()", "Map()"),

      (emptyMap, "Map(a -> 1)", """Map("a" -> 1)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> 1L)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> e)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> BigInt(1))"""),

      (Map("a" -> "1"), "Map(a -> 2)", """Map("a" -> (longMap.head._2 + 1))"""),
      (Map("a" -> "1"), "Map(a -> 2)", """Map("a" -> (longMap.head._2 + 1L))"""),
      (Map("a" -> "1"), "Map(a -> 2)", """Map("a" -> (longMap.head._2 + e))"""),
      (Map("a" -> "1"), "Map(a -> 2)", """Map("a" -> (longMap.head._2 + BigInt(1)))"""),

      (Map("a" -> "1"), "Map(a -> 1, b -> 2)", """longMap + ("b" -> 2)"""),
      (Map("a" -> "1"), "Map(a -> 1, b -> 2)", """longMap + ("b" -> 2L)"""),
      (Map("a" -> "1"), "Map(a -> 1, b -> 2)", """longMap + ("b" -> BigInt(2))"""),
      (Map("a" -> "1"), "Map(a -> 1, b -> 2)", """longMap + ("b" -> (e + 1))"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "longMap", "Long", "mapDouble", 3), testData)
  }


  def bigIntMap(): Unit = {
    val testData = List(
      // Floating point number types non-compatible
      //      (emptyMap, "Map(a -> 1.1)", """(Map("a" -> 1.1f)"""),
      //      (emptyMap, "Map(a -> 1.1)", """(Map("a" -> 1.1)"""),
      //      (emptyMap, "Map(a -> 1.1)", """(Map("a" -> BigDecimal(1.1))"""),

      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Nil"),
      (emptyMap, "Map()", "Map()"),

      (emptyMap, "Map(a -> 1)", """Map("a" -> 1)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> 1L)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> e)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> BigInt(1))"""),

      (Map("a" -> "1"), "Map(a -> 2)", """Map("a" -> (bigIntMap.head._2 + 1))"""),
      (Map("a" -> "1"), "Map(a -> 2)", """Map("a" -> (bigIntMap.head._2 + 1L))"""),
      (Map("a" -> "1"), "Map(a -> 2)", """Map("a" -> (bigIntMap.head._2 + e))"""),
      (Map("a" -> "1"), "Map(a -> 2)", """Map("a" -> (bigIntMap.head._2 + BigInt(1)))"""),

      (Map("a" -> "1"), "Map(a -> 1, b -> 2)", """bigIntMap + ("b" -> 2)"""),
      (Map("a" -> "1"), "Map(a -> 1, b -> 2)", """bigIntMap + ("b" -> 2L)"""),
      (Map("a" -> "1"), "Map(a -> 1, b -> 2)", """bigIntMap + ("b" -> BigInt(2))"""),
      (Map("a" -> "1"), "Map(a -> 1, b -> 2)", """bigIntMap + ("b" -> (e + 1))"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bigIntMap", "BigInt", "mapDouble", 3), testData)
  }

  BigInt(1)


  def floatMap(): Unit = {
    val testData = List(
      // Use Double instead of Float
      (emptyMap, s"""$iae Please use Double instead of Float in expression `Map("a" -> 4.2f)` to get correct floating point precision.""", """Map("a" -> 4.2f)"""),

      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Nil"),
      (emptyMap, "Map()", "Map()"),
      (emptyMap, "Map(a -> 1)", """Map("a" -> 1)"""),

      (emptyMap, "Map(a -> 1)", """Map("a" -> 1)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> 1L)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> e)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> BigInt(1))"""),
      (emptyMap, "Map(a -> 1.4)", """Map("a" -> 1.4)"""),
      (emptyMap, "Map(a -> 1.4)", """Map("a" -> BigDecimal(1.4))"""),

      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (floatMap.head._2 + 1))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (floatMap.head._2 + 1L))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (floatMap.head._2 + e))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (floatMap.head._2 + BigInt(1)))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.4)", """Map("a" -> (floatMap.head._2 + 1.3))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.4)", """Map("a" -> (floatMap.head._2 + BigDecimal(1.3)))"""),


      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """floatMap + ("b" -> 1)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """floatMap + ("b" -> 1L)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """floatMap + ("b" -> e)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """floatMap + ("b" -> BigInt(1))"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1.3)", """floatMap + ("b" -> 1.3)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 2.3)", """floatMap + ("b" -> 2.3)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 2.3)", """floatMap + ("b" -> BigDecimal(2.3))"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "floatMap", "Float", "mapDouble", 3), testData)
  }


  def doubleMap(): Unit = {
    val testData = List(
      // Use Double instead of Float
      (emptyMap, s"""$iae Please use Double instead of Float in expression `Map("a" -> 4.2f)` to get correct floating point precision.""", """Map("a" -> 4.2f)"""),

      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Nil"),
      (emptyMap, "Map()", "Map()"),
      (emptyMap, "Map(a -> 1)", """Map("a" -> 1)"""),

      (emptyMap, "Map(a -> 1)", """Map("a" -> 1)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> 1L)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> e)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> BigInt(1))"""),
      (emptyMap, "Map(a -> 1.4)", """Map("a" -> 1.4)"""),
      (emptyMap, "Map(a -> 1.4)", """Map("a" -> BigDecimal(1.4))"""),

      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (doubleMap.head._2 + 1))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (doubleMap.head._2 + 1L))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (doubleMap.head._2 + e))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (doubleMap.head._2 + BigInt(1)))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.4)", """Map("a" -> (doubleMap.head._2 + 1.3))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.4)", """Map("a" -> (doubleMap.head._2 + BigDecimal(1.3)))"""),

      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """doubleMap + ("b" -> 1)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """doubleMap + ("b" -> 1L)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """doubleMap + ("b" -> e)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """doubleMap + ("b" -> BigInt(1))"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1.3)", """doubleMap + ("b" -> 1.3)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 2.3)", """doubleMap + ("b" -> 2.3)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 2.3)", """doubleMap + ("b" -> BigDecimal(2.3))"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "doubleMap", "Double", "mapDouble", 3), testData)
  }


  def bigDecimalMap(): Unit = {
    val testData = List(
      // Use Double instead of Float
      (emptyMap, s"""$iae Please use Double instead of Float in expression `Map("a" -> 4.2f)` to get correct floating point precision.""", """Map("a" -> 4.2f)"""),

      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Nil"),
      (emptyMap, "Map()", "Map()"),
      (emptyMap, "Map(a -> 1)", """Map("a" -> 1)"""),

      (emptyMap, "Map(a -> 1)", """Map("a" -> 1)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> 1L)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> e)"""),
      (emptyMap, "Map(a -> 1)", """Map("a" -> BigInt(1))"""),
      (emptyMap, "Map(a -> 1.4)", """Map("a" -> 1.4)"""),
      (emptyMap, "Map(a -> 1.4)", """Map("a" -> BigDecimal(1.4))"""),

      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (bigDecMap.head._2 + 1))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (bigDecMap.head._2 + 1L))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (bigDecMap.head._2 + e))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.1)", """Map("a" -> (bigDecMap.head._2 + BigInt(1)))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.4)", """Map("a" -> (bigDecMap.head._2 + 1.3))"""),
      (Map("a" -> "1.1"), "Map(a -> 2.4)", """Map("a" -> (bigDecMap.head._2 + BigDecimal(1.3)))"""),

      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """bigDecMap + ("b" -> 1)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """bigDecMap + ("b" -> 1L)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """bigDecMap + ("b" -> e)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1)", """bigDecMap + ("b" -> BigInt(1))"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 1.3)", """bigDecMap + ("b" -> 1.3)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 2.3)", """bigDecMap + ("b" -> 2.3)"""),
      (Map("a" -> "1.1"), "Map(a -> 1.1, b -> 2.3)", """bigDecMap + ("b" -> BigDecimal(2.3))"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "bigDecMap", "BigDecimal", "mapDouble", 3), testData)
  }


  def stringMap(): Unit = {
    val testData = List(
      // Options not used for card many lists. Instead a list can be either
      // empty (~ None) or having values (~ Some(list)).
      // Use mandatory notation without '$' postfix ($ is stripped):
      (Map("a" -> "a"), "Map(a -> a)", "strMap$"),
      // Treated same as
      (Map("a" -> "a"), "Map(a -> a)", "strMap"),

      // Unchanged
      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Nil"),
      (emptyMap, "Map()", "Map()"),
      (Map("a" -> "a"), "Map(a -> a)", "strMap"),
      (Map("a" -> "a", "b" -> "b"), "Map(a -> a, b -> b)", "strMap"),

      // Add
      (emptyMap, "Map(a -> a)", """strMap + ("a" -> "a")"""),
      (Map("a" -> "a"), "Map(a -> a, b -> b)", """strMap + ("b" -> "b")"""),

      // Update
      (Map("a" -> "a", "b" -> "b"), "Map(a -> ax, b -> bx)", """strMap.map { case (k, v) => k -> (v + "x")}"""),

      // Retract element
      (Map("a" -> "a", "b" -> "b", "c" -> "c"), "Map(a -> a, b -> b)", "strMap.init"),
      (Map("a" -> "a", "b" -> "b", "c" -> "c"), "Map(b -> b, c -> c)", "strMap.tail"),

      // Retract all
      (Map("a" -> "a", "b" -> "b"), "Map()", ""),
      (Map("a" -> "a", "b" -> "b"), "Map()", "Nil"),
      (Map("a" -> "a", "b" -> "b"), "Map()", "Map()"),
      (Map("a" -> "a", "b" -> "b"), "Map()", "strMap.take(0)"),

      // String treatment
      (Map("a" -> "a2"), "Map(a -> )", """Map("a" -> "")"""),
      (Map("a" -> "a2"), "Map(a ->  )", """Map("a" -> " ")"""),
      (Map("a" -> "a2"), "Map(a -> \n )", """Map("a" -> "\n ")"""),
      (Map("a" -> "a2"), "Map(a ->  \n )", """Map("a" -> " \n ")"""),
      (Map("a" -> "a3"), "Map(a -> Code is:\na3)", """Map("a" -> ("Code is:\n" + strMap.head._2))"""),
      (Map("a" -> "a3"), "Map(a -> a34)", """Map("a" -> (strMap.head._2 + 4))"""),

      (Map("a" -> "a3"), "Map(a -> a3, b -> b)", """strMap + ("b" -> "b")"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "strMap", "String", "mapString", 3), testData)
  }


  def boolMap(): Unit = {
    val testData = List(
      (Map("a" -> "true"), "Map(a -> false)", """Map("a" -> false)"""),
      (Map("a" -> "true"), "Map(a -> true)", "boolMap"),
      (Map("a" -> "true"), "Map(a -> false)", """Map("a" -> !boolMap.head._2)"""),
      (Map("a" -> "true"), "Map(a -> false)", "boolMap.map { case (k, v) => k -> !v }"),
      (Map("a" -> "true"), "Map(a -> false)", """Map("a" -> (boolMap.head._2 && 7 == 8))"""),
      (Map("a" -> "true"), "Map(a -> true)", """Map("a" -> (boolMap.head._2 || 7 == 8))"""),
      (Map("a" -> "true"), "Map(a -> true, b -> false)", """boolMap + ("b" -> false)"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "boolMap", "Boolean", "mapString", 3), testData)
  }


  def dateMap(): Unit = {
    val oneDate  = Map("a" -> LocalDateTime.of(2001, 3, 5, 7, 9, 11).toString)
    val testData = List(
      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Map()"),
      (emptyMap, "Map()", "dateMap$"),

      // For more examples, see card 1 date examples

      // 3 ways of constructing dateMap
      (emptyMap, "Map(a -> 2001-03-05T07:09:11)", """Map("a" -> LocalDateTime.of(2001, 3, 5, 7, 9, 11))"""),
      (emptyMap, "Map(a -> 2001-03-05T07:09:11)", """Map("a" -> d(2001, 3, 5, 7, 9, 11))"""),
      // Implicitly creating LocalDateTime from String
      (emptyMap, "Map(a -> 2001-03-05T07:09:11)", """Map("a" -> "2001-3-5 7:9:11")"""),

      (oneDate, "Map(a -> 2001-03-05T07:09:11)", "dateMap"),
      (oneDate, "Map(a -> 2301-03-05T07:09:11)", "dateMap.map { case (k, v) => (k, v.plusYears(300)) }"),
      (oneDate, "Map(a -> 2001-03-05T07:09:11)", """Map("a" -> dateMap.head._2)"""),
      (oneDate, "Map(a -> 2001-03-05T07:09:12)", """Map("a" -> dateMap.head._2.plusSeconds(1))"""),

      (oneDate, "Map(a -> 2001-03-05T07:09:11, b -> 2002-01-01T00:00)", """dateMap + ("b" -> LocalDateTime.of(2002, 1, 1, 0, 0))"""),
      (oneDate, "Map(a -> 2001-03-05T07:09:11, b -> 2002-01-01T00:00)", """dateMap + ("b" -> d(2002))"""),
      (oneDate, "Map(a -> 2001-03-05T07:09:11, b -> 2002-01-01T00:00)", """dateMap + ("b" -> "2002")"""),

      (oneDate, "Map(a -> 2001-03-05T07:09:11, b -> 2018-01-01T00:00)", """dateMap ++ Map("b" -> "2018")"""),
      (
        oneDate,
        s"$iae Invalid String/LocalDateValue pair (b -> 2018) of types class java.lang.String/class java.lang.Short. " +
          s"Key should be a String and value of type String or LocalDateTime.",
        """dateMap ++ Map("b" -> 2018)"""
      ),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "dateMap", "Date", "mapString", 3), testData)
  }


  def uuidMap(): Unit = {
    // Use stable uuid's to allow compiler to cache
    val uuid1 = "aba20f8e-8e79-475c-b8d1-df11f57b29ba"
    val uuid2 = "b0924388-9c1b-4ce4-92ac-7b8d2b01beec"

    val testData = List(
      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Map()"),

      (Map("a" -> uuid1), s"Map(a -> $uuid1)", "uuidMap"),
      (Map("a" -> uuid2), s"Map(a -> $uuid2)", s"""Map("a" -> UUID.fromString("$uuid2"))"""),
      // Implicitly creating UUID from String
      (Map("a" -> uuid2), s"Map(a -> $uuid2)", s"""Map("a" -> "$uuid2")"""),

      (Map("a" -> uuid1), s"Map(a -> $uuid1, b -> $uuid2)", s"""uuidMap + ("b" -> UUID.fromString("$uuid2"))"""),
      (Map("a" -> uuid1), s"Map(a -> $uuid1, b -> $uuid2)", s"""uuidMap + ("b" -> "$uuid2")"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uuidMap", "UUID", "mapString", 3), testData)
  }


  def uriMap(): Unit = {
    val uri1     = "uri1"
    val uri2     = "uri2"
    val testData = List(
      (emptyMap, "Map()", ""),
      (emptyMap, "Map()", "Map()"),

      (Map("a" -> uri1), s"Map(a -> uri1)", "uriMap"),
      (Map("a" -> uri2), s"Map(a -> uri2)", """Map("a" -> new URI("uri2"))"""),
      // Implicitly creating URI from String
      (Map("a" -> uri2), s"Map(a -> uri2)", """Map("a" -> "uri2")"""),

      (Map("a" -> uri1), s"Map(a -> uri1, b -> uri2)", s"""uriMap + ("b" -> new URI("uri2"))"""),
      (Map("a" -> uri1), s"Map(a -> uri1, b -> uri2)", s"""uriMap + ("b" -> "uri2")"""),
    )
    Compiler(Col(2, 0, "Ns", "Ns", "uriMap", "URI", "mapString", 3), testData)
  }
}