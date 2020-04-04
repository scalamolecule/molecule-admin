package moleculeadmin.client.app.logic.query.keyEvents

import org.scalajs.dom.document
import moleculeadmin.client.app.logic.query.QueryState._

trait MarkerToggling {

  def getCellId: String = {
    val nodeList = document.querySelectorAll(":hover")
    val idAttr   = nodeList.item(nodeList.length - 1).attributes.getNamedItem("id")
    if (idAttr == null) "" else idAttr.value
  }

  def toggleStar(): Unit = {
    toggler = "s"
    starTogglers.get(getCellId).foreach(_())
  }

  def toggleFlag(): Unit = {
    toggler = "f"
    flagTogglers.get(getCellId).foreach(_())
  }

  def toggleCheck(): Unit = {
    toggler = "c"
    checkTogglers.get(getCellId).foreach(_())
  }
}
