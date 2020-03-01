package moleculeadmin.clienttest
import molecule.util.DateHandling
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.groupEdit.ops.ScalaCode
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.client.scalafiddle.ScalaFiddle
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


      1 ==> 2


    }

  }
}