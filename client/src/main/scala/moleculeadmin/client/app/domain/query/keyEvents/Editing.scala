package moleculeadmin.client.app.domain.query.keyEvents
import moleculeadmin.client.app.domain.query.QueryState.editCellId
import org.scalajs.dom.raw.{HTMLInputElement, KeyboardEvent}
import org.scalajs.dom.{Node, document, window}
import scalatags.JsDom.all.li


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
    val curCell = document.activeElement
    val card    = curCell.getAttribute("card")
    val cls     = curCell.getAttribute("class")
    if (!(
      card != null && (card == "2" || card == "3") ||
        cls != null && (cls == "str" || cls == "filter" || cls == "edit"))
    ) {
      // Prevent internal line shifts in non-String cells
      e.preventDefault()
      window.alert(
        """Shift-Return can only be used to create line shifts for:
          |- Cardinality-many attributes
          |- Cardinality-one String attributes
          |""".stripMargin)
    } else {
      // Otherwise allow soft new line within cell
    }
  }

  def multilineAddItem(e: KeyboardEvent): Unit = {
    val curCell = document.activeElement
    // Create new empty String item (since we have multiline values)
    if (curCell.getAttribute("class") == "items") {
      // Prevent line shift
      e.preventDefault()
      // add list item
      val ul = curCell.firstChild
      ul.appendChild(li().render)
      // Set caret in new item
      val range = document.createRange
      val sel   = window.getSelection
      range.setStart(ul.childNodes.item(ul.childNodes.length - 1), 0)
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