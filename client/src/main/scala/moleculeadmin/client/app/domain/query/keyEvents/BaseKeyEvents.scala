package moleculeadmin.client.app.domain.query.keyEvents

import org.scalajs.dom.document
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

  def blur(): Unit = document.activeElement.asInstanceOf[HTMLInputElement].blur()

  def noBottomScroll(e: KeyboardEvent): Unit = {
    // prevent default scroll to bottom
    e.preventDefault()
  }
}
