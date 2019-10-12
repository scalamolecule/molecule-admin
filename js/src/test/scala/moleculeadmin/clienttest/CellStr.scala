package moleculeadmin.clienttest

import moleculeadmin.client.app.element.AppElements
import scalatags.JsDom.all._
import utest._


object CellStr extends TestSuite with AppElements {


  val tests = Tests {

    test("str") {
      _str2frags("a") ==> List(StringFrag("a"))

      _html2str("a") ==> "a"
    }

    test("empty str") {
      _str2frags("{}") ==> List(StringFrag("{}"))

    }

    test("spaces") {
      // Spaces replaced with &nbsp;
      _str2frags("{ }") ==> List(StringFrag("{\u00a0}"))

      _str2frags("{  }") ==> List(StringFrag("{\u00a0\u00a0}"))


    }

    test("line shift") {
      _str2frags("{ \n }") ==> List(StringFrag("{\u00a0"), br, StringFrag("\u00a0}"))
    }
  }
}