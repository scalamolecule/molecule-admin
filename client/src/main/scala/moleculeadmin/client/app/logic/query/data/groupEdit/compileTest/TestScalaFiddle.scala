package moleculeadmin.client.app.logic.query.data.groupEdit.compileTest
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.testdata.ExampleData
import moleculeadmin.shared.util.HelpersAdmin

trait TestScalaFiddle extends HelpersAdmin with ExampleData {

  // Long treated as String for BigInt consumption
  // to avoid precision issues on js side
  val eid       = "1"
  val eCol: Col = Col(0, 0, "Ns", "Ns", "e", "datom", "double", 1)

  val iae = "java.lang.IllegalArgumentException:"
}

object TestScalaFiddle {

  /*
  OBS:
  Comment out specific test groups below to test multiple compilations against
  a running scalafiddle-core router and server (See the ScalaFiddle.scala for
  more details on running those).

  Each test data line in a test group contains 3 elements:
  - initial data
  - expected data
  - Right hand side of a Scala lambda expression

  The Scala expression is sent to ScalaFiddle, compiled to JS and then applied
  to the initial data and the result is compared to the expected data.

  Note that initially the ScalaFiddle server will send back error-400's for
  all calls except the current one. Then just refresh until all code snippets
  have been cached.
  */

  val card1 = Card1
  //  card1.int()
  //  card1.long()
  //  card1.bigInt()
  //  card1.float()
  //  card1.double()
  //  card1.bigDec()
  //  card1.string()
  //  card1.bool()
  //  card1.date()
  //  card1.uuid()
  //  card1.uri()

  val card2 = Card2
  //  card2.ints()
  //  card2.longs()
  //  card2.bigInts()
  //  card2.floats()
  //  card2.doubles()
  //  card2.bigDec()
  //  card2.strings()
  //  card2.bools()
  //  card2.dates()
  //  card2.uuids()
  //  card2.uris()

  val card3 = Card3
  //  card3.intMap()
  //  card3.longMap()
  //  card3.bigIntMap()
  //  card3.floatMap()
  //  card3.doubleMap()
  //  card3.bigDecimalMap()
  //  card3.stringMap()
  //  card3.boolMap()
  //  card3.dateMap()
  //  card3.uuidMap()
  //  card3.uriMap()
}