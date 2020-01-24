package moleculeadmin.sharedtest.query.data

import moleculeadmin.shared.ast.query.{Col, Filter}
import moleculeadmin.shared.ops.query.data.FilterFactory
import utest._


/*
  match
  /regex
  i/regex case insensitive
  !inverted
  !/inverted regex
  !i/inverted case insensitive regex
  v => // syntax indicating this is a Scala expression to be compiled!
  v => v match {
    case "asc"  => 42
    case "desc" => 43
  }
*/

object FilterCreate extends TestSuite with FilterFactory {

  val num = List[Double](-10, -3, -2, -1, 0, 1, 2, 3, 10)
  val dec = List[Double](-10.1, -3.1, -2.1, -1.1, 0, 1.1, 2.1, 3.1, 10.1)
  val str = List("a", "b", "c", "d")


  def mkFilter(colIndex: Int, attrType: String, colType: String, filterExpr: String): Option[Filter[_]] = createFilter(
    Col(colIndex, 0, "", "", "", attrType, colType, 1, false, Nil, "", "", "", 0), filterExpr
  )


  def testDoubleExpr(filterExpr: String, attrType: String) = {
    mkFilter(0, attrType, "double", filterExpr) match {
      case Some(f: Filter[_]) if attrType == "Int"    =>
        num.map(Some(_)) filter f.asInstanceOf[Filter[Double]].pred

      case Some(f: Filter[_]) if attrType == "Double" =>
        dec.map(Some(_)) filter f.asInstanceOf[Filter[Double]].pred

      case _                                          =>
        Nil
    }
  }

  def number(filterExpr: String) = testDoubleExpr(filterExpr, "Int")
  def decimal(filterExpr: String) = testDoubleExpr(filterExpr, "Double")

  val NotMatchOrWrongExpr = Nil

  val tests = Tests {

    test("General") {

      // Unrecognized expr matches nothing
      // expr printed to console (no exception thrown)
      number("xq@#%") ==> NotMatchOrWrongExpr

      // expr tokens trimmed
      number(" 1 ") ==> List(Some(1))
      number(" 1 \n \n 2 , 3 ") ==> List(Some(1), Some(2), Some(3))

      // no space between minus sign and number
      number("-1") ==> List(Some(-1))
      number("- 1") ==> NotMatchOrWrongExpr
    }


    test("Int, Long, ref") {

      // Unrecognized expr matches nothing
      // expr printed to console (no exception thrown)
      number("xq@#%") ==> NotMatchOrWrongExpr

      // expr tokens trimmed
      number(" 1 ") ==> List(Some(1))
      number(" 1 \n \n 2 , 3 ") ==> List(Some(1), Some(2), Some(3))

      // comma-separated and line shift have same OR semantics
      number("1, 3") ==> List(Some(1), Some(3))
      number("1\n3") ==> List(Some(1), Some(3))

      // comparison
      number(">2") ==> List(Some(3), Some(10))
      number("> 2") ==> List(Some(3), Some(10))
      number(">  2") ==> List(Some(3), Some(10))
      number(">-2") ==> List(Some(-1), Some(0), Some(1), Some(2), Some(3), Some(10))
      number("> -2") ==> List(Some(-1), Some(0), Some(1), Some(2), Some(3), Some(10))
      number("> - 2") ==> NotMatchOrWrongExpr

      number("<2") ==> List(Some(-10), Some(-3), Some(-2), Some(-1), Some(-0), Some(1))
      number("<-2") ==> List(Some(-10), Some(-3))

      number(">=2") ==> List(Some(2), Some(3), Some(10))
      number(">=-2") ==> List(Some(-2), Some(-1), Some(0), Some(1), Some(2), Some(3), Some(10))

      number("<=2") ==> List(Some(-10), Some(-3), Some(-2), Some(-1), Some(0), Some(1), Some(2))
      number("<= -2") ==> List(Some(-10), Some(-3), Some(-2))

      // range
      number("1-3") ==> List(Some(1), Some(2), Some(3))
      number("1  -  3") ==> List(Some(1), Some(2), Some(3))

      number("-1-1") ==> List(Some(-1), Some(0), Some(1))
      number("-1- 1") ==> List(Some(-1), Some(0), Some(1))
      number("-1 -1") ==> List(Some(-1), Some(0), Some(1))
      number("-1 - 1") ==> List(Some(-1), Some(0), Some(1))

      number("-3--1") ==> List(Some(-3), Some(-2), Some(-1))
      number("-3 - -1") ==> List(Some(-3), Some(-2), Some(-1))
    }


    test("Float, Double") {

      // comparison
      decimal(">2") ==> List(Some(2.1), Some(3.1), Some(10.1))
      decimal(">2.") ==> List(Some(2.1), Some(3.1), Some(10.1))
      decimal(">2.0") ==> List(Some(2.1), Some(3.1), Some(10.1))
      decimal(">2.1") ==> List(Some(3.1), Some(10.1))
      decimal("> 2.1") ==> List(Some(3.1), Some(10.1))
      decimal(">  2.1") ==> List(Some(3.1), Some(10.1))
      decimal(">-2.1") ==> List(Some(-1.1), Some(0), Some(1.1), Some(2.1), Some(3.1), Some(10.1))
      decimal("> -2.1") ==> List(Some(-1.1), Some(0), Some(1.1), Some(2.1), Some(3.1), Some(10.1))
      decimal("> - 2.1") ==> NotMatchOrWrongExpr

      decimal("<2") ==> List(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1))
      decimal("<2.1") ==> List(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1))
      decimal("<2.2") ==> List(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1), Some(2.1))
      decimal("<-2") ==> List(Some(-10.1), Some(-3.1), Some(-2.1))
      decimal("<-2.1") ==> List(Some(-10.1), Some(-3.1))

      decimal(">=2") ==> List(Some(2.1), Some(3.1), Some(10.1))
      decimal(">=2.1") ==> List(Some(2.1), Some(3.1), Some(10.1))
      decimal(">=-2") ==> List(Some(-1.1), Some(0), Some(1.1), Some(2.1), Some(3.1), Some(10.1))
      decimal(">=-2.1") ==> List(Some(-2.1), Some(-1.1), Some(0), Some(1.1), Some(2.1), Some(3.1), Some(10.1))

      decimal("<=2") ==> List(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1))
      decimal("<=2.1") ==> List(Some(-10.1), Some(-3.1), Some(-2.1), Some(-1.1), Some(0), Some(1.1), Some(2.1))
      decimal("<= -2") ==> List(Some(-10.1), Some(-3.1), Some(-2.1))
      decimal("<= -2.1") ==> List(Some(-10.1), Some(-3.1), Some(-2.1))
      decimal("<= -2.2") ==> List(Some(-10.1), Some(-3.1))


      // range
      decimal("1.1-3.1") ==> List(Some(1.1), Some(2.1), Some(3.1))
      decimal("1.1  -  3.1") ==> List(Some(1.1), Some(2.1), Some(3.1))

      decimal("-1.1-1.1") ==> List(Some(-1.1), Some(0), Some(1.1))
      decimal("-1.1- 1.1") ==> List(Some(-1.1), Some(0), Some(1.1))
      decimal("-1.1 -1.1") ==> List(Some(-1.1), Some(0), Some(1.1))
      decimal("-1.1 - 1.1") ==> List(Some(-1.1), Some(0), Some(1.1))

      decimal("-3.1--1.1") ==> List(Some(-3.1), Some(-2.1), Some(-1.1))
      decimal("-3.1 - -1.1") ==> List(Some(-3.1), Some(-2.1), Some(-1.1))
    }


    test("date") {

    }

    test("string") {

    }
  }

}
