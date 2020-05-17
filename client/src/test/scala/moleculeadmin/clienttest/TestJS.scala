package moleculeadmin.clienttest
import util.client.rx.RxBindings
import molecule.util.DateHandling
import moleculeadmin.client.app.logic.QueryClient
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.groupEdit.ops.ScalaCode
import moleculeadmin.client.scalafiddle.ScalaFiddle
import moleculeadmin.shared.ast.query.Col
import org.scalajs.dom.ext.Ajax
//import org.scalajs.dom._
import scalatags.JsDom.all._

import rx.Ctx
//import scalatags.Text.all._
import utest._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
//import scala.concurrent.{ExecutionContext, Future}
//import scala.scalajs.js
//import scala.scalajs.js.annotation.JSGlobal
import org.scalajs.dom.document


// sbt> moleculeAdminJS/test
// sbt> moleculeAdminJS/testOnly -- moleculeadmin.clienttest.TestJS

//import TestUtil._

object TestJS extends TestSuite with RxBindings with DateHandling {


  implicit val ctx: Ctx.Owner = rx.Ctx.Owner.safe()


  val editCol =
    Col(2, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "edit", "", 0)

  columns() = List(
    Col(0, 0, "Ns", "Ns", "e", "datom", "double", 1, false, Seq(), "", "", "", 0),
    Col(1, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "orig", "", 0),
    editCol
  )


  val tests = Tests {

    test("html") {

//      import bundle.short._
//      val page = html(
//        body(
////          script(s"QueryClient.load('CoreTest')"),
////          div(id := "scriptWrapper"),
//          "Hello World"
//        )
//      ).render

      val page = html(body("Hello World")).render

      page.firstChild.textContent ==> "Hello World"

//      page.render.children.length ==> 0

//      QueryClient.load("CoreTest")


      1 ==> 2


    }

  }
}