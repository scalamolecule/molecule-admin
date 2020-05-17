package moleculeadmin.clienttest

import org.scalajs.dom.html.Html
import scalatags.Text
import scalatags.Text.all._
import utest._

object HtmlTest extends TestSuite {

  val tests = Tests {

    test("scalatags.Text.all._") {
      val page: Text.TypedTag[String] = html(body("Hello World"))
      println(page.toString())
      page.asInstanceOf[Html].firstChild.textContent ==> "Hello World"
    }
  }
}