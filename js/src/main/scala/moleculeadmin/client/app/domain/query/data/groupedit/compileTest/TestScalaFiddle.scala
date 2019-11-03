package moleculeadmin.client.app.domain.query.data.groupedit.compileTest
import moleculeadmin.client.app.domain.query.data.groupedit.compileTest.Card1.testCount
import moleculeadmin.client.scalafiddle.ScalaFiddle
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.testdata.ExampleData
import moleculeadmin.shared.util.HelpersAdmin
import scala.concurrent.Future
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

trait TestScalaFiddle extends HelpersAdmin with ExampleData {

  // Long treated as String for BigInt consumption
  // to avoid precision issues on js side
  val eid       = "1"
  val eCol: Col = Col(0, 0, "Ns", "Ns", "e", "datom", "double", 1)

  val iae = "java.lang.IllegalArgumentException:"
}

object TestScalaFiddle {

  // OBS: make sure to scroll up in the browser console output
  // to see eventual errors (since all tests run in parallel)

  // Isolate tests of interest by commenting out others

  val card1 = Card1
  //  card1.int()
  //  card1.long()
  //  card1.bigInt()
  //  card1.float()
  //  card1.double()
  //  card1.bigDecimal()
  //  card1.string()
  //  card1.bool()
  //  card1.date()
  //  card1.uuid()
  //  card1.uri()


  val card2 = Card2
  card2.ints()
  //    card2.longs()
  //    card2.bigInts()
  //    card2.floats()
  //    card2.doubles()
  //    card2.bigDecimals()
  //    card2.strings()
  //    card2.bools()
  //    card2.dates()
  //    card2.uuids()
  //    card2.uris()


}