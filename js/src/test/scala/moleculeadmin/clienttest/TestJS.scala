package moleculeadmin.clienttest
import moleculeadmin.shared.Shared
import org.scalajs.dom.html
import utest._

// sbt> fooJS/test
object TestJS extends TestSuite {

  val tests = Tests {
    test("test") {
      println("a", "b")
      Shared.confirm("Test js")
    }


    test("scalafiddle") {


    }

  }
}