package moleculeadmin.clienttest
import org.scalajs.dom.html.Html
import scalatags.JsDom
import scalatags.JsDom.all._
import utest._

object JsDomTest extends TestSuite {

  val tests = Tests {

    test("scalatags.JsDom.all._") {
      val page: JsDom.TypedTag[Html] = html(body("Hello World"))
      println(page.toString())
      page.render.firstChild.textContent ==> "Hello World"
    }

  }
}