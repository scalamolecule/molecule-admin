package moleculeadmin.clienttest

import org.scalajs.dom
import org.scalajs.dom.document


object TutorialApp {
  def main(args: Array[String]): Unit = {
    appendPar(document.body, "Hello World")
  }


  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    parNode.textContent = text
    targetNode.appendChild(parNode)
  }
}