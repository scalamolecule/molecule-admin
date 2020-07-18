package moleculeadmin.clienttest_sbt_only

import org.scalajs.dom.document
import scalatags.JsDom.all._
import utest._

object Tester extends TestSuite {


  document.body.appendChild(span("hej").render)

  def tests = Tests {
    test("hej") {
      assert(document.body.textContent == "hejx")
    }
  }
}