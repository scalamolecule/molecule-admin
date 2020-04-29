package moleculeadmin.client.app.logic.query.data.groupEdit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.html.query.datatable.HeadElements
import moleculeadmin.client.app.logic.query.QueryState.{db, editExprs}
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.Col
import org.scalajs.dom.html.Anchor
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{document, window}
import scalatags.JsDom
import scalatags.jsdom.Frag
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.timers.setTimeout


case class EditExprs(col: Col) extends HeadElements {

  val Col(colIndex, _, _, nsFull, attr, _, _, card, opt, _, _, _, _, _, _) = col

  val fullAttr = s":$nsFull/$attr"

  val defaultEditExpr: String = {
    def s(i: Int) = "\u00a0" * i
    card match {
      case 1 if opt => attr
      case 1        => s"Some($attr)"
      case 2        => s"$attr.map(v => v)"
      case 3        =>
        s"""$attr.map {
           |${s(2)}case (k, v) => (k, v)
           |}""".stripMargin
    }
  }

  def editExprItems: List[Frag] = {
    mkEditExprItems(
      defaultEditExpr :: editExprs.getOrElse(s":$nsFull/$attr", Nil)
    )
  }

  def mkEditExprItems(editExprs: List[String]): List[JsDom.TypedTag[Anchor]] = {
    editExprs.distinct.zipWithIndex.map { case (editExpr, i) =>
      val eeId = editExprId(i)
      editExprItem(
        i == 0,
        eeId,
        editExpr,
        retract(editExpr, eeId),
        pick(editExpr)
      )
    }
  }

  def editDropdownId: String = s"editDropdown-$colIndex"
  def editExprId(i: Int): String = s"editExpr-$colIndex-$i"

  // Hack to keep dropdown open when retracting edit expressions
  var pickingEditExprs = true

  def pick(editExpr: String): () => Unit = {
    () => {
      if (pickingEditExprs) {
        // Funny hack - move the element to cancel the hover state!
        // After hover is cancelled we reposition to allow new hover
        val dropDown = document.getElementById(editDropdownId)
        val style    = dropDown.getAttribute("style")
        dropDown.setAttribute("style", style + "left:2000px")
        setTimeout(100) {
          dropDown.setAttribute("style", style)
        }

        // Set edit expression in filter cell
        val editCell = document.getElementById("filter-" + colIndex)
        editCell.innerHTML = editExpr.replace("\n", "<br>")
        editCell.asInstanceOf[HTMLInputElement].focus()
      } else {
        pickingEditExprs = true
      }
    }
  }

  def retract(editExpr: String, editExprId: String): () => Unit = () => {
    val dropdown     = document.getElementById(editDropdownId)
    val editExprElem = document.getElementById(editExprId)
    dropdown.removeChild(editExprElem)

    // retract in db
    queryWireAjax().retractEditExpr(db, fullAttr, editExpr).call().foreach {
      case Right(okMsg) => println(okMsg)
      case Left(err)    => window.alert(
        "Error retracting edit expression:\n" + err
      )
    }

    // End hover state
    pickingEditExprs = false
  }

  def upsert(editExpr: String): Unit = {

    // Update client cache with latest expr first
    val curEditExprs = editExprs.getOrElse(fullAttr, List.empty[String])
    val newEditExprs = editExpr :: curEditExprs.filterNot(_ == editExpr)
    editExprs(fullAttr) = newEditExprs

    // Update edit dropdown
    val dropdown                = document.getElementById(editDropdownId)
    val items                   = dropdown.childNodes
    val (save, cancel, divider) = (items(0), items(1), items(2))
    dropdown.innerHTML = ""
    dropdown.appendChild(save)
    dropdown.appendChild(cancel)
    dropdown.appendChild(divider)

    mkEditExprItems(defaultEditExpr :: newEditExprs.take(10))
      .foreach(item => dropdown.appendChild(item.render))

    // Cache in meta db
    queryWireAjax().upsertEditExpr(db, fullAttr, editExpr).call().foreach {
      case Right(okMsg) => println(okMsg)
      case Left(err)    => window.alert(
        "Error caching edit expression:\n" + err
      )
    }
  }
}
