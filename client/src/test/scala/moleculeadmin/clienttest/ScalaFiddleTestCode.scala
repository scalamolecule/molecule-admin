package moleculeadmin.clienttest

import moleculeadmin.client.app.logic.query.data.groupEdit.ops.ScalaCode
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.shared.ast.query.Col
import scalatags.JsDom.all._
import utest._


object ScalaFiddleTestCode extends TestSuite with AppElements {


  val tests = Tests {

    test("shared code outside lambda") {

      val cols = List(
        Col(0, 0, "Ns", "Ns", "e", "datom", "double", 1, false, Seq(), "", "", "", 0, ""),
        Col(1, 0, "Ns", "Ns", "long", "Long", "double", 1, false, Seq(), "", "orig", "", 0, ""),
        Col(2, 0, "Ns", "Ns", "long", "Long", "double", 1, false, Seq(), "", "edit", "", 0, ""))


      ScalaCode(cols, cols.last,
        """var i = 0
          |---
          |i += 1
          |Some(long + i)""".stripMargin
      ).get ==>
        """import scalajs.js
          |import js.annotation.{JSExportTopLevel, JSExport}
          |import js.JSConverters._
          |import java.time._
          |import java.util.{Date, UUID}
          |import java.net.URI
          |
          |@JSExportTopLevel("ScalaFiddle")
          |object ScalaFiddle {
          |
          |  @JSExport
          |  val lambda: js.Tuple2[String, String] => js.Tuple2[js.UndefOr[String], String] = {
          |    case js.Tuple2(e: String, long: String) =>
          |      try {
          |        val result: Option[BigInt] = process(
          |          BigInt(e),
          |          BigInt(long)
          |        )
          |        js.Tuple2(result.map(_.toString).orUndefined, "")
          |      } catch {
          |        case e: Throwable => js.Tuple2(Option.empty[String].orUndefined, e.toString)
          |      }
          |  }
          |
          |  val process: (BigInt, BigInt) => Option[BigInt] = {
          |    var i = 0
          |    (e: BigInt, long: BigInt) => {
          |      i += 1
          |      Some(long + i)
          |    }
          |  }
          |}
          |// $ScalaVersion 2.12
          |// $ScalaJSVersion 0.6""".stripMargin


    }
  }
}