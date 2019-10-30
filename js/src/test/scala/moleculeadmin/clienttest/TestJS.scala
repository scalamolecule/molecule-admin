package moleculeadmin.clienttest
import molecule.util.DateHandling
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.groupedit.ops.ScalaCode
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.client.scalafiddle.ScalafiddleApi
import moleculeadmin.shared.ast.query.Col
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html
import rx.Ctx
import utest._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
//import scala.concurrent.{ExecutionContext, Future}
//import scala.scalajs.js
//import scala.scalajs.js.annotation.JSGlobal


// sbt> moleculeAdminJS/test
// sbt> moleculeAdminJS/testOnly -- moleculeadmin.clienttest.TestJS


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

    test("moleculeadmin/client/scalafiddle") {

      val rhs                                 = "str + 1"
      val scalaCode  : String                 = ScalaCode(editCol, rhs).get
      val scalafiddle: ScalafiddleApi[String] = ScalafiddleApi[String](scalaCode)

      val futFn: Future[(Any, Any) => String] = scalafiddle.lambda2

      println("yeah")

      futFn.foreach { fn =>
        println(fn(7L, "a"))
//        fn(7L, "a") ==> "a1"
      }



      //      val xx = Ajax.post(
      //        "http://localhost:8880/compile?opt=fast",
      //        scalaCode
      //      )
      //
      ////      doc.appendChild(page.render)
      //
      //
      //      doc.body.lastChild.tagName.toString ==> "H1"
      //      doc.body.lastChild.innerHTML.toString ==> "Test"


      //      val col      = Col(2, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "edit", "", 0)
      //      val filterId = "hi"

      //GroupEdit(col, filterId).string()

    }

  }
}