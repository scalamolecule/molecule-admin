package moleculeadmin.client.app.logic.query.keyEvents

import org.scalajs.dom.document
import org.scalajs.dom.html.{TableCell, TableRow}
import org.scalajs.dom.raw.{HTMLInputElement, KeyboardEvent}
import scala.scalajs.js.timers.setTimeout


trait BaseKeyEvents {

  private var firstNumber  = -1
  private var secondNumber = -1
  private val numberMs     = 300 // todo: setting?

  def numberInputs(n0: String, action: Int => Unit): Unit = {
    val n = n0.toInt
    if (firstNumber == -1) {
      firstNumber = n
      setTimeout(numberMs) {
        val index = if (secondNumber == -1)
          firstNumber - 1
        else
          firstNumber * 10 + secondNumber - 1
        if (index >= 0)
          action(index)
        firstNumber = -1
        secondNumber = -1
      }
    } else if (secondNumber == -1) {
      secondNumber = n
    }
  }

  def blur(): Unit = {
    val cell = document.activeElement.asInstanceOf[HTMLInputElement]
    cell.blur()
    cell.parentNode.asInstanceOf[TableRow].className = "view"
  }

  def noScrollToBottom(e: KeyboardEvent): Unit = {
    // prevent default scroll to bottom
    e.preventDefault()
  }

  def toggle(id: String): Unit = {
    val el = document.getElementById(id)
    if (el != null) {
      val style = el.getAttribute("style")
      if (style == null || style.isEmpty)
        el.setAttribute("style", "display:none;")
      else
        el.removeAttribute("style")
    }
  }
}
