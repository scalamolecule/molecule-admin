package moleculeadmin.client.app.logic.query.keyEvents

import moleculeadmin.client.app.logic.query.QueryState.editCellId
import org.scalajs.dom.raw.{HTMLInputElement, HTMLUListElement, KeyboardEvent}
import org.scalajs.dom.{Node, document, window}
import scalatags.JsDom.all._


trait Editing {

  def getColNo(id: String): Int =
    if (id.startsWith("grouped-cell-")) 1 else id.substring(4, 6).trim.toInt

  def selectContent(elem: Node): Unit = {
    val range = document.createRange()
    range.selectNodeContents(elem);
    val sel = window.getSelection();
    sel.removeAllRanges();
    sel.addRange(range);
  }

  def cellUp(): Unit = {
    val curCell = document.activeElement
    val curRow  = curCell.parentNode
    val prevRow = curRow.previousSibling
    if (prevRow != null) {
      val colNo     = getColNo(curCell.id)
      val cellAbove = prevRow.childNodes.item(colNo)
      selectContent(cellAbove.asInstanceOf[HTMLInputElement])
    }
  }

  def cellDown(): Unit = {
    val curCell = document.activeElement
    val curRow  = curCell.parentNode
    val nextRow = curRow.nextSibling
    if (nextRow != null) {
      val colNo     = getColNo(curCell.id)
      val cellBelow = nextRow.childNodes.item(colNo)
      selectContent(cellBelow.asInstanceOf[HTMLInputElement])
    }
  }

  def deleteItem(e: KeyboardEvent): Unit = {
    val curCell = document.activeElement
    // Avoid deleting item code
    if (curCell.getAttribute("class") == "items") {
      // Find caret position in cell
      val range         = window.getSelection.getRangeAt(0)
      val preCaretRange = range.cloneRange()
      preCaretRange.selectNodeContents(curCell)
      preCaretRange.setEnd(range.endContainer, range.endOffset)
      val caretOffset = preCaretRange.toString.length
      if (caretOffset == 0) {
        // Prevent deleting first item
        // (would break code structure that we depend on)
        e.preventDefault()
      }
    }
  }

  def multilineSoftNewLine(e: KeyboardEvent): Unit = {
    val curCell         = document.activeElement
    val card            = curCell.getAttribute("card")
    val isCardMany      = card != null && (card == "2" || card == "3")
    val cls             = curCell.getAttribute("class")
    val clss            = if (cls != null) cls.split(" ").toSeq else Seq.empty[String]
    val acceptMultiLine = clss.intersect(Seq("str", "input")).nonEmpty
    if (isCardMany || acceptMultiLine) {
      // Allow soft new line within cell
    } else {
      // Prevent internal line shifts in non-String cells
      e.preventDefault()
      window.alert(
        """Shift-Return can only be used to create line shifts for:
          |- Cardinality-many attributes
          |- Cardinality-one String attributes
          |""".stripMargin)
    }
  }

  def multilineAddItem(e: KeyboardEvent): Unit = {
    val curCell = document.activeElement
    // Create new empty String item (since we have multiline values)
    if (curCell.getAttribute("class") == "items") {
      // Prevent line shift
      e.preventDefault()
      val node0: Node = curCell.firstChild
      val uList: Node = if (node0.isInstanceOf[HTMLUListElement]) {
        // add list item to existing unordered list
        node0.appendChild(li().render)
        node0
      } else {
        // Remove empty unordered list in empty cell
        curCell.removeChild(curCell.lastElementChild)
        // Place text in unordered list and add new empty item to continue editing
        val curVs    = curCell.innerHTML.split("<br>").toList
        val newUList = ul(
          li(curVs.flatMap(v => Seq(v: Frag, br)).init),
          li()
        ).render
        // Replace cell with uList
        curCell.innerHTML = ""
        curCell.appendChild(newUList)
        newUList
      }
      // Set caret in new item
      val range       = document.createRange
      val sel         = window.getSelection
      range.setStart(uList.childNodes.item(uList.childNodes.length - 1), 0)
      range.collapse(true)
      sel.removeAllRanges
      sel.addRange(range)
    }
  }

  def saveEdit(e: KeyboardEvent): Unit = {
    // prevent creating new line within cell
    e.preventDefault()
    val curCell = document.activeElement
    val curRow  = curCell.parentNode
    val nextRow = curRow.nextSibling
    editCellId = curCell.id
    if (nextRow != null) {
      val colNo     = getColNo(editCellId)
      val cellBelow = nextRow.childNodes.item(colNo)
      // Select content of cell below
      // Fires blur-callback (save) on current cell
      selectContent(cellBelow.asInstanceOf[HTMLInputElement])
      if (editCellId.isEmpty) {
        // Re-select content in original cell if invalid data
        selectContent(curCell)
      }
    } else {
      curCell.asInstanceOf[HTMLInputElement].blur()
    }
    editCellId = ""
  }
}