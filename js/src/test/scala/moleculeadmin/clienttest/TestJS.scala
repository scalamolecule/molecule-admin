package moleculeadmin.clienttest
import molecule.util.DateHandling
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.groupedit.GroupEdit
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.{Col, Filter, QueryCache, QueryResult}
import moleculeadmin.shared.ast.tree.Tree
import rx.Ctx
import utest._
import scalatags.JsDom.all._
import org.scalajs.dom.{Document, document}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import js.Dynamic.global

// sbt> moleculeAdminJS/test
// sbt> moleculeAdminJS/testOnly -- moleculeadmin.clienttest.TestJS

//@js.native
//@JSGlobal
//object TestDoc extends Document with js.Object {
//  def main(args: Array[String]): Unit = {
//    println("Hello world!")
//  }
//}

object TestJS extends TestSuite with RxBindings with DateHandling {




  implicit val ctx: Ctx.Owner = rx.Ctx.Owner.safe()

  modelElements() = Nil

  val str: List[Array[Option[String]]] = List(
    Array(Some("a"), Some("b")),
    Array(Some("a"), Some("b"))
  )
  val num: List[Array[Option[Double]]] = List(
    Array(Some(1), Some(2))
  )

  columns() = List(
    Col(0, 0, "Ns", "Ns", "e", "datom", "double", 1, false, Seq(), "", "", "", 0),
    Col(1, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "orig", "", 0),
    Col(2, 0, "Ns", "Ns", "str", "String", "string", 1, false, Seq(), "", "edit", "", 0))

  val qr = QueryResult(
    str, num, Nil, Nil, Nil, Nil,
    Map(
      0 -> 0,
      1 -> 0,
      2 -> 1,
    ),
    2, 2, 0
  )

  queryCache() = Seq(QueryCache(
    Nil,
    Tree(Nil, Nil, Nil, Nil, Nil, Nil),
    "",
    qr,
    columns.now,
    Array(0, 1),
    Map.empty[Int, Filter[_]],
    Array.empty[Int]
  ))


  val tests = Tests {

    test("moleculeadmin/client/scalafiddle") {

//      def go = println()

      val page = html(
        body(
          "hi"
          //script(s"QueryClient.load('CoreTest')")
        )
      )

      println(js.typeOf(js.Dynamic.global))
      js.typeOf(js.Dynamic.global.document) ==> "undefined"

//      val doc = global.document
//      val h1 = doc.createElement("h1")
//      h1.innerHTML = "Test"
//      doc.body.appendChild(h1)
//
//
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