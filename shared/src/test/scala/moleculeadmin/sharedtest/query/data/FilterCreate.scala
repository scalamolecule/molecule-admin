package moleculeadmin.sharedtest.query.data

import moleculeadmin.shared.ast.query.{Col, Filter}
import moleculeadmin.shared.ops.query.data.FilterFactory
import utest._


object FilterCreate extends TestSuite with FilterFactory {

  def mkFilter(
    colIndex: Int,
    attrType: String,
    colType: String,
    filterExpr: String
  ): Option[Filter[_]] =
    createFilter(Col(colIndex, 0, "", "", "", attrType, colType, 1), filterExpr)

  def testDoubleExpr(vs: Seq[Option[Double]], filterExpr: String, attrType: String): Seq[Option[Double]] = {
    mkFilter(0, attrType, "double", filterExpr).fold(vs)(f =>
      vs filter f.asInstanceOf[Filter[Double]].pred
    )
  }

  def testStringExpr(vs: Seq[Option[String]], filterExpr: String, attrType: String): Seq[Option[String]] = {
    mkFilter(0, attrType, "string", filterExpr).fold(vs)(f =>
      vs filter f.asInstanceOf[Filter[String]].pred
    )
  }

  val NotMatchOrWrongExpr = Nil


  val tests = Tests {
    test("String") {
      val strings  = Seq(Some("Apple"), Some("Banana"), Some("Citrus"), None)
      val noFilter = strings
      def string(filterExpr: String): Seq[Option[String]] =
        testStringExpr(strings, filterExpr, "String")

      // No filtering
      string("") ==> noFilter

      // All values
      string("+") ==> Seq(Some("Apple"), Some("Banana"), Some("Citrus"))
      // All nulls
      string("-") ==> Seq(None)

      // Contains needle (case-sensitive match)
      string("a") ==> Seq(Some("Banana"))
      string("A") ==> Seq(Some("Apple"))

      // Beware, spaces are matched too
      string("a ") ==> Seq()

      // Doesn't contain needle (case-sensitive)
      string("!a") ==> Seq(Some("Apple"), Some("Citrus"))
      string("!A") ==> Seq(Some("Banana"), Some("Citrus"))

      // Regex (case-sensitive)
      string("/.*a.*/") ==> Seq(Some("Banana")) // no Apple

      // Last '/' can be omitted
      string("/.*a.*") ==> Seq(Some("Banana"))
      string("/B.*") ==> Seq(Some("Banana"))
      string("/.*a") ==> Seq(Some("Banana"))

      // case-insensitive regex
      string("i/.*a.*") ==> Seq(Some("Apple"), Some("Banana"))

      // inverted regex
      string("!/.*a.*") ==> Seq(Some("Apple"), Some("Citrus"))

      // inverted case-insensitive regex
      string("!i/.*a.*") ==> Seq(Some("Citrus"))


      // Comparison

      // Capital letters come before lower-case
      string("<b") ==> Seq(Some("Apple"), Some("Banana"), Some("Citrus"))
      string("<B") ==> Seq(Some("Apple"))
      string(">B") ==> Seq(Some("Banana"), Some("Citrus"))
      string(">Banana") ==> Seq(Some("Citrus"))
      string(">=Banana") ==> Seq(Some("Banana"), Some("Citrus"))
      string("<Banana") ==> Seq(Some("Apple"))
      string("<=Banana") ==> Seq(Some("Apple"), Some("Banana"))


      // Multiple needles

      // OR logic applied
      string("p") ==> Seq(Some("Apple"))
      string("u") ==> Seq(Some("Citrus"))
      string("!u") ==> Seq(Some("Apple"), Some("Banana"))

      // comma-separated
      string("p,u") ==> Seq(Some("Apple"), Some("Citrus"))
      string("p,!u") ==> Seq(Some("Apple"), Some("Banana"))
      // Beware, spaces are matched too (' u' has no match)
      string("p, u") ==> Seq(Some("Apple"))

      // Newline separated
      string("-\np") ==> Seq(Some("Apple"), None)
      string("p\n-") ==> Seq(Some("Apple"), None)
      string("!p\n-") ==> Seq(Some("Banana"), Some("Citrus"), None)


      // Empty spaces ................

      val strings2 = Seq(Some("a"), Some(""), Some(" "), Some("\n"), Some(" \n "), Some("a\nb"))
      def string2(filterExpr: String) = testStringExpr(strings2, filterExpr, "String")
      val noFilter2 = strings2

      // Needle matches multiple lines (OR logic)
      string2("a") ==> Seq(Some("a"), Some("a\nb"))

      // Match with AND logic to enforce all lines matching needle
      string2("/.*a.*/") ==> Seq(Some("a"))


      // Likewise, multi-line needles match each newline value of needle
      string2("a\nb") ==> Seq(Some("a"), Some("a\nb"))
      // Empty "sub-needle" is discarded (only matching `a`)
      string2("a\n") ==> Seq(Some("a"), Some("a\nb"))

      string2("{}") ==> Seq(Some(""))
      string2("{ }") ==> Seq(Some(" "))

      // Matching newline not supported
      string2("\n") ==> noFilter2
    }


    test("Date") {
      val a        = Some("2001-01-01")
      val b        = Some("2002-01-01")
      val c        = Some("2002-02-01")
      val d        = Some("2002-02-02")
      val e        = Some("2002-02-02 02:00")
      val f        = Some("2002-02-02 02:02")
      val g        = Some("2002-02-02 02:02:02")
      val h        = Some("2002-02-02 02:02:02.222")
      val i        = Some("2002-02-02 02:02:02.333")
      val dates    = Seq(a, b, c, d, e, f, g, h, i, None)
      val noFilter = dates
      def date(filterExpr: String) = testStringExpr(dates, filterExpr, "Date")

      date("") ==> noFilter

      date("+") ==> Seq(a, b, c, d, e, f, g, h, i)
      date("-") ==> Seq(None)

      date("2") ==> Seq(a, b, c, d, e, f, g, h, i)
      date("20") ==> Seq(a, b, c, d, e, f, g, h, i)
      date("200") ==> Seq(a, b, c, d, e, f, g, h, i)
      date("2001") ==> Seq(a)
      date("2001-01") ==> Seq(a)
      date("2001-01-01") ==> Seq(a)

      date("2002") ==> Seq(b, c, d, e, f, g, h, i)

      date("2002-1") ==> Seq()
      date("2002-0") ==> Seq(b, c, d, e, f, g, h, i)
      date("2002-01") ==> Seq(b)

      date("2002-02") ==> Seq(c, d, e, f, g, h, i)
      date("2002-02-01") ==> Seq(c)
      date("2002-02-02") ==> Seq(d, e, f, g, h, i)

      date("2002-02-02 02") ==> Seq(e, f, g, h, i)
      date("2002-02-02 02:02") ==> Seq(f, g, h, i)
      date("2002-02-02 02:02:02") ==> Seq(g, h, i)
      date("2002-02-02 02:02:02.222") ==> Seq(h)


      // Ranges (space between operator and needle is optional)

      date(">=2001") ==> Seq(a, b, c, d, e, f, g, h, i)
      date(">= 2001") ==> Seq(a, b, c, d, e, f, g, h, i)
      date(">= 2002") ==> Seq(b, c, d, e, f, g, h, i)
      date(">= 2002-01") ==> Seq(b, c, d, e, f, g, h, i)
      date(">= 2002-02") ==> Seq(c, d, e, f, g, h, i)
      date(">= 2002-02-02") ==> Seq(d, e, f, g, h, i)
      date(">= 2002-02-02 02:00") ==> Seq(e, f, g, h, i)
      date(">= 2002-02-02 02:02") ==> Seq(f, g, h, i)
      date(">= 2002-02-02 02:02:02") ==> Seq(g, h, i)
      date(">= 2002-02-02 02:02:02.222") ==> Seq(h, i)
      date(">= 2002-02-02 02:02:02.333") ==> Seq(i)

      date("> 2001") ==> Seq(b, c, d, e, f, g, h, i)
      date("> 2002") ==> Seq(c, d, e, f, g, h, i)
      date("> 2002-01") ==> Seq(c, d, e, f, g, h, i)
      date("> 2002-02") ==> Seq(d, e, f, g, h, i)
      date("> 2002-02-02") ==> Seq(e, f, g, h, i)
      date("> 2002-02-02 02:00") ==> Seq(f, g, h, i)
      date("> 2002-02-02 02:02") ==> Seq(g, h, i)
      date("> 2002-02-02 02:02:02") ==> Seq(h, i)
      date("> 2002-02-02 02:02:02.222") ==> Seq(i)
      date("> 2002-02-02 02:02:02.333") ==> Seq()

      date("<= 2001") ==> Seq(a)
      date("<= 2002") ==> Seq(a, b)
      date("<= 2002-01") ==> Seq(a, b)
      date("<= 2002-02") ==> Seq(a, b, c)
      date("<= 2002-02-02") ==> Seq(a, b, c, d)
      date("<= 2002-02-02 02:00") ==> Seq(a, b, c, d, e)
      date("<= 2002-02-02 02:02") ==> Seq(a, b, c, d, e, f)
      date("<= 2002-02-02 02:02:02") ==> Seq(a, b, c, d, e, f, g)
      date("<= 2002-02-02 02:02:02.222") ==> Seq(a, b, c, d, e, f, g, h)
      date("<= 2002-02-02 02:02:02.333") ==> Seq(a, b, c, d, e, f, g, h, i)

      date("< 2001") ==> Seq()
      date("< 2002") ==> Seq(a)
      date("< 2002-01") ==> Seq(a)
      date("< 2002-02") ==> Seq(a, b)
      date("< 2002-02-02") ==> Seq(a, b, c)
      date("< 2002-02-02 02:00") ==> Seq(a, b, c, d)
      date("< 2002-02-02 02:02") ==> Seq(a, b, c, d, e)
      date("< 2002-02-02 02:02:02") ==> Seq(a, b, c, d, e, f)
      date("< 2002-02-02 02:02:02.222") ==> Seq(a, b, c, d, e, f, g)
      date("< 2002-02-02 02:02:02.333") ==> Seq(a, b, c, d, e, f, g, h)
      date("< 2002-02-02 02:02:02.444") ==> Seq(a, b, c, d, e, f, g, h, i)


      date("2001--2002") ==> Seq(a, b)
      date("2001--2002-01") ==> Seq(a, b)
      date("2001--2002-02") ==> Seq(a, b, c)
      date("2001--2002-02-01") ==> Seq(a, b, c)
      date("2001--2002-02-02") ==> Seq(a, b, c, d)
      date("2001--2002-02-02 02:00") ==> Seq(a, b, c, d, e)
      date("2001--2002-02-02 02:02") ==> Seq(a, b, c, d, e, f)
      date("2001--2002-02-02 02:02:02") ==> Seq(a, b, c, d, e, f, g)
      date("2001--2002-02-02 02:02:02.222") ==> Seq(a, b, c, d, e, f, g, h)
      date("2001--2002-02-02 02:02:02.333") ==> Seq(a, b, c, d, e, f, g, h, i)

      date("2002-02-01--2002-02-02") ==> Seq(c, d)
      date("2002-02-01--2002-02-02 02:02:02") ==> Seq(c, d, e, f, g)
    }


    test("Boolean") {
      val bools    = Seq(Some("true"), Some("false"), None)
      val noFilter = bools
      def boolean(filterExpr: String) = testStringExpr(bools, filterExpr, "Boolean")

      boolean("") ==> noFilter

      boolean("-") ==> Seq(None)
      boolean("+") ==> Seq(Some("true"), Some("false"))

      boolean("true") ==> Seq(Some("true"))
      boolean("tru") ==> Seq(Some("true"))
      boolean("tr") ==> Seq(Some("true"))
      boolean("t") ==> Seq(Some("true"))
      boolean("1") ==> Seq(Some("true"))

      boolean("false") ==> Seq(Some("false"))
      boolean("fals") ==> Seq(Some("false"))
      boolean("fal") ==> Seq(Some("false"))
      boolean("fa") ==> Seq(Some("false"))
      boolean("f") ==> Seq(Some("false"))
      boolean("0") ==> Seq(Some("false"))
    }


    test("Int, Long, ref") {
      val numbers  = Seq[Double](-10, -3, -2, -1, 0, 1, 2, 3, 10).map(Some(_))
      val noFilter = numbers
      def number(filterExpr: String) = testDoubleExpr(numbers, filterExpr, "Int")

      // General

      // Unrecognized expr matches nothing
      // expr printed to console (no exception thrown)
      number("xq@#%") ==> noFilter

      // expr tokens trimmed
      number(" 1 ") ==> Seq(Some(1))
      number(" 1 \n \n 2 , 3 ") ==> Seq(Some(1), Some(2), Some(3))

      // no space between minus sign and number
      number("-1") ==> Seq(Some(-1))
      number("- 1") ==> noFilter

      // Unrecognized expr matches nothing
      // expr printed to console (no exception thrown)
      number("xq@#%") ==> noFilter

      // expr tokens trimmed
      number(" 1 ") ==> Seq(Some(1))
      number(" 1 \n \n 2 , 3 ") ==> Seq(Some(1), Some(2), Some(3))

      // comma-separated and line shift have same OR semantics
      number("1, 3") ==> Seq(Some(1), Some(3))
      number("1\n3") ==> Seq(Some(1), Some(3))

      // comparison
      number(">2") ==> Seq(Some(3), Some(10))
      number("> 2") ==> Seq(Some(3), Some(10))
      number(">  2") ==> Seq(Some(3), Some(10))
      number(">-2") ==> Seq(Some(-1), Some(0), Some(1), Some(2), Some(3), Some(10))
      number("> -2") ==> Seq(Some(-1), Some(0), Some(1), Some(2), Some(3), Some(10))
      number("> - 2") ==> noFilter

      number("<2") ==> Seq(Some(-10), Some(-3), Some(-2), Some(-1), Some(-0), Some(1))
      number("<-2") ==> Seq(Some(-10), Some(-3))

      number(">=2") ==> Seq(Some(2), Some(3), Some(10))
      number(">=-2") ==> Seq(Some(-2), Some(-1), Some(0), Some(1), Some(2), Some(3), Some(10))

      number("<=2") ==> Seq(Some(-10), Some(-3), Some(-2), Some(-1), Some(0), Some(1), Some(2))
      number("<= -2") ==> Seq(Some(-10), Some(-3), Some(-2))

      // range
      number("1-3") ==> Seq(Some(1), Some(2), Some(3))
      number("1  -  3") ==> Seq(Some(1), Some(2), Some(3))

      number("-1-1") ==> Seq(Some(-1), Some(0), Some(1))
      number("-1- 1") ==> Seq(Some(-1), Some(0), Some(1))
      number("-1 -1") ==> Seq(Some(-1), Some(0), Some(1))
      number("-1 - 1") ==> Seq(Some(-1), Some(0), Some(1))

      number("-3 - -1") ==> Seq(Some(-3), Some(-2), Some(-1))
      number("-3--1") ==> Seq(Some(-3), Some(-2), Some(-1))
    }


    test("Float, Double") {
      val decimals = Seq[Double](-10.1, -3.1, -2.1, -1.1, 0, 1.1, 2.1, 3.1, 10.1).map(Some(_))
      val noFilter = decimals
      def decimal(filterExpr: String) = testDoubleExpr(decimals, filterExpr, "Double")

      // comparison
      decimal(">2") ==> Seq(Some(2.1), Some(3.1), Some(10.1))
      decimal(">2.") ==> Seq(Some(2.1), Some(3.1), Some(10.1))
      decimal(">2.0") ==> Seq(Some(2.1), Some(3.1), Some(10.1))
      decimal(">2.1") ==> Seq(Some(3.1), Some(10.1))
      decimal("> 2.1") ==> Seq(Some(3.1), Some(10.1))
      decimal(">  2.1") ==> Seq(Some(3.1), Some(10.1))
      decimal(">-2.1") ==> Seq(Some(-1.1), Some(0), Some(1.1), Some(2.1), Some(3.1), Some(10.1))
      decimal("> -2.1") ==> Seq(Some(-1.1), Some(0), Some(1.1), Some(2.1), Some(3.1), Some(10.1))
      decimal("> - 2.1") ==> noFilter

      decimal("<2") ==> Seq(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1))
      decimal("<2.1") ==> Seq(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1))
      decimal("<2.2") ==> Seq(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1), Some(2.1))
      decimal("<-2") ==> Seq(Some(-10.1), Some(-3.1), Some(-2.1))
      decimal("<-2.1") ==> Seq(Some(-10.1), Some(-3.1))

      decimal(">=2") ==> Seq(Some(2.1), Some(3.1), Some(10.1))
      decimal(">=2.1") ==> Seq(Some(2.1), Some(3.1), Some(10.1))
      decimal(">=-2") ==> Seq(Some(-1.1), Some(0), Some(1.1), Some(2.1), Some(3.1), Some(10.1))
      decimal(">=-2.1") ==> Seq(Some(-2.1), Some(-1.1), Some(0), Some(1.1), Some(2.1), Some(3.1), Some(10.1))

      decimal("<=2") ==> Seq(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1))
      decimal("<=2.1") ==> Seq(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1), Some(2.1))
      decimal("<= -2") ==> Seq(Some(-10.1), Some(-3.1), Some(-2.1))
      decimal("<= -2.1") ==> Seq(Some(-10.1), Some(-3.1), Some(-2.1))
      decimal("<= -2.2") ==> Seq(Some(-10.1), Some(-3.1))


      // range
      decimal("1.1-3.1") ==> Seq(Some(1.1), Some(2.1), Some(3.1))
      decimal("1.1  -  3.1") ==> Seq(Some(1.1), Some(2.1), Some(3.1))

      decimal("-1.1-1.1") ==> Seq(Some(-1.1), Some(0), Some(1.1))
      decimal("-1.1- 1.1") ==> Seq(Some(-1.1), Some(0), Some(1.1))
      decimal("-1.1 -1.1") ==> Seq(Some(-1.1), Some(0), Some(1.1))
      decimal("-1.1 - 1.1") ==> Seq(Some(-1.1), Some(0), Some(1.1))

      decimal("-3.1 - -1.1") ==> Seq(Some(-3.1), Some(-2.1), Some(-1.1))
      decimal("-3.1--1.1") ==> Seq(Some(-3.1), Some(-2.1), Some(-1.1))
    }


    test("BigInt") {
      val a = Some("-10")
      val b = Some("-3")
      val c = Some("-2")
      val d = Some("-1")
      val e = Some("0")
      val f = Some("1")
      val g = Some("2")
      val h = Some("3")
      val i = Some("10")

      val bigInts  = Seq[Option[String]](a, b, c, d, e, f, g, h, i)
      val noFilter = bigInts
      def bigInt(filterExpr: String) = testStringExpr(bigInts, filterExpr, "BigInt")

      bigInt(">2") ==> Seq(h, i)
      bigInt("> 2") ==> Seq(h, i)
      bigInt(">  2") ==> Seq(h, i)
      bigInt(">-2") ==> Seq(d, e, f, g, h, i)
      bigInt("> -2") ==> Seq(d, e, f, g, h, i)
      bigInt("> - 2") ==> noFilter

      bigInt("<2") ==> Seq(a, b, c, d, e, f)
      bigInt("<-2") ==> Seq(a, b)
      bigInt("<-2") ==> Seq(a, b)

      bigInt(">=2") ==> Seq(g, h, i)
      bigInt(">=-2") ==> Seq(c, d, e, f, g, h, i)

      bigInt("<=2") ==> Seq(a, b, c, d, e, f, g)
      bigInt("<= -2") ==> Seq(a, b, c)

      bigInt("1-3") ==> Seq(f, g, h)
      bigInt("1  -  3") ==> Seq(f, g, h)

      bigInt("-1-1") ==> Seq(d, e, f)
      bigInt("-1- 1") ==> Seq(d, e, f)
      bigInt("-1 -1") ==> Seq(d, e, f)
      bigInt("-1 - 1") ==> Seq(d, e, f)

      bigInt("-3 - -1") ==> Seq(b, c, d)
      bigInt("-3--1") ==> Seq(b, c, d)
    }


    test("BigDecimal") {
      val a = Some("-10.1")
      val b = Some("-3.1")
      val c = Some("-2.1")
      val d = Some("-1.1")
      val e = Some("0")
      val f = Some("1.1")
      val g = Some("2.1")
      val h = Some("3.1")
      val i = Some("10.1")

      val bigDecimals = Seq[Option[String]](a, b, c, d, e, f, g, h, i)
      val noFilter    = bigDecimals
      def bigDecimal(filterExpr: String) = testStringExpr(bigDecimals, filterExpr, "BigDecimal")

      bigDecimal(">2") ==> Seq(g, h, i)
      bigDecimal(">2.") ==> Seq(g, h, i)
      bigDecimal(">2.0") ==> Seq(g, h, i)
      bigDecimal(">2.1") ==> Seq(h, i)
      bigDecimal("> 2.1") ==> Seq(h, i)
      bigDecimal(">  2.1") ==> Seq(h, i)
      bigDecimal(">-2.1") ==> Seq(d, e, f, g, h, i)
      bigDecimal("> -2.1") ==> Seq(d, e, f, g, h, i)
      bigDecimal("> - 2.1") ==> noFilter

      bigDecimal("<2") ==> Seq(a, b, c, d, e, f)
      bigDecimal("<2.1") ==> Seq(a, b, c, d, e, f)
      bigDecimal("<2.2") ==> Seq(a, b, c, d, e, f, g)
      bigDecimal("<-2") ==> Seq(a, b, c)
      bigDecimal("<-2.1") ==> Seq(a, b)

      bigDecimal(">=2") ==> Seq(g, h, i)
      bigDecimal(">=2.1") ==> Seq(g, h, i)
      bigDecimal(">=-2") ==> Seq(d, e, f, g, h, i)
      bigDecimal(">=-2.1") ==> Seq(c, d, e, f, g, h, i)

      bigDecimal("<=2") ==> Seq(a, b, c, d, e, f)
      bigDecimal("<=2.1") ==> Seq(a, b, c, d, e, f, g)
      bigDecimal("<= -2") ==> Seq(a, b, c)
      bigDecimal("<= -2.1") ==> Seq(a, b, c)
      bigDecimal("<= -2.2") ==> Seq(a, b)

      bigDecimal("1.1-3.1") ==> Seq(f, g, h)
      bigDecimal("1.1  -  3.1") ==> Seq(f, g, h)

      bigDecimal("-1.1-1.1") ==> Seq(d, e, f)
      bigDecimal("-1.1- 1.1") ==> Seq(d, e, f)
      bigDecimal("-1.1 -1.1") ==> Seq(d, e, f)
      bigDecimal("-1.1 - 1.1") ==> Seq(d, e, f)

      bigDecimal("-3.1 - -1.1") ==> Seq(b, c, d)
      bigDecimal("-3.1--1.1") ==> Seq(b, c, d)
    }
  }
}
