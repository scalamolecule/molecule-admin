package moleculeadmin.client.app.logic.query.keyEvents

import moleculeadmin.client.app.logic.query.QueryState.editCellId
import org.scalajs.dom.html.TableRow
import org.scalajs.dom.raw.{Element, HTMLInputElement, HTMLUListElement, KeyboardEvent}
import org.scalajs.dom.{Node, document, window}
import rx.Ctx
import scalatags.JsDom.all._


trait Editing extends Paging {

  def getColNo(id: String): Int =
    if (id.startsWith("grouped-cell-")) 1 else id.substring(4, 6).trim.toInt

  def selectContent(elem: Node): Unit = {
    val range = document.createRange()
    range.selectNodeContents(elem);
    val sel = window.getSelection();
    sel.removeAllRanges();
    sel.addRange(range);
  }

  def markNewRow(curRow: Node, newRow: Node): Unit = {
    curRow.asInstanceOf[TableRow].className = "view"
    newRow.asInstanceOf[TableRow].className = "edit"
  }

  def markRow(curRow: Node): Unit = {
    curRow.asInstanceOf[TableRow].className = "edit"
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

  def saveEditMoveUp(e: KeyboardEvent): Unit = {
    // prevent creating new line within cell
    e.preventDefault()
    val curCell = document.activeElement
    val curRow  = curCell.parentNode
    val rowAbove = curRow.previousSibling
    editCellId = curCell.id
    if (rowAbove != null) {
      val colNo     = getColNo(editCellId)
      val cellBelow = rowAbove.childNodes.item(colNo)
      // Select content of cell below
      // Fires blur-callback (save) on current cell
      selectContent(cellBelow.asInstanceOf[HTMLInputElement])
      if (editCellId.isEmpty) {
        // Re-select content in original cell if invalid data
        selectContent(curCell)
      } else {
        markNewRow(curRow, rowAbove)
      }
    } else {
      curCell.asInstanceOf[HTMLInputElement].blur()
    }
    // reset current edit cell id
    editCellId = ""
  }

  def saveEditMoveDown(e: KeyboardEvent): Unit = {
    // prevent creating new line within cell
    e.preventDefault()
    val curCell = document.activeElement
    val curRow  = curCell.parentNode
    val rowUnder = curRow.nextSibling
    editCellId = curCell.id
    if (rowUnder != null) {
      val colNo     = getColNo(editCellId)
      val cellBelow = rowUnder.childNodes.item(colNo)
      // Select content of cell below
      // Fires blur-callback (save) on current cell
      selectContent(cellBelow.asInstanceOf[HTMLInputElement])
      if (editCellId.isEmpty) {
        // Re-select content in original cell if invalid data
        selectContent(curCell)
      } else {
        markNewRow(curRow, rowUnder)
      }
    } else {
      curCell.asInstanceOf[HTMLInputElement].blur()
    }
    // reset current edit cell id
    editCellId = ""
  }


  def saveEditMoveForward(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent immediately moving to next cell
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    editCellId = curCell.id

    def nextEditableCell(testCell: HTMLInputElement): Option[HTMLInputElement] = {
      val sibling = testCell.nextSibling.asInstanceOf[HTMLInputElement]
      if (sibling == null)
        None
      else if (sibling.isContentEditable)
        Some(sibling)
      else
        nextEditableCell(sibling)
    }

    nextEditableCell(curCell).fold {
      // Trigger update with blur
      curCell.blur()
      // Go to first selectable cell in next row
      val curRow  = curCell.parentNode
      val rowUnder = curRow.nextSibling
      if (rowUnder != null) {
        val firstCellNextRow = rowUnder.firstChild.asInstanceOf[HTMLInputElement]
        selectContent(nextEditableCell(firstCellNextRow).get)
        markNewRow(curRow, rowUnder)
      } else {
        if (isLast) {
          // Re-select cell to not loose focus
          selectContent(curCell)
          markRow(curRow)
        } else {
          // Go to next page and select first editable cell on first row
          nextPage
          val firstRow  = document.getElementById("tableBody")
            .firstChild.asInstanceOf[TableRow]
          val firstCell = firstRow.firstChild.asInstanceOf[HTMLInputElement]
          markRow(firstRow)
          selectContent(nextEditableCell(firstCell).get)
        }

      }
    } { nextCell =>
      // Select content of next editable cell
      // Fires blur-callback (save) on current cell
      selectContent(nextCell)
      if (editCellId.isEmpty) {
        // Re-select content in original cell if invalid data
        selectContent(curCell)
      } else {
        markRow(curCell.parentNode)
      }
    }
    editCellId = ""
  }


  def saveEditMoveBackwards(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent immediately moving to next cell
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    editCellId = curCell.id

    def previousEditableCell(testCell: HTMLInputElement): Option[HTMLInputElement] = {
      val sibling = testCell.previousSibling.asInstanceOf[HTMLInputElement]
      if (sibling == null)
        None
      else if (sibling.isContentEditable)
        Some(sibling)
      else
        previousEditableCell(sibling)
    }

    previousEditableCell(curCell).fold {
      // Trigger update with blur
      curCell.blur()
      // Go to last selectable cell in previous row
      val curRow  = curCell.parentNode
      val rowAbove = curRow.previousSibling
      if (rowAbove != null) {
        val lastCellPrevRow = rowAbove.lastChild.asInstanceOf[HTMLInputElement]
        selectContent(previousEditableCell(lastCellPrevRow).get)
        markNewRow(curRow, rowAbove)
      } else {
        if (isFirst) {
          // Re-select cell to not loose focus
          selectContent(curCell)
          markRow(curRow)
        } else {
          // Go to previous page and select last editable cell on last row
          prevPage
          val lastRow  = document.getElementById("tableBody")
            .lastChild.asInstanceOf[TableRow]
          val lastCell = lastRow.lastChild.asInstanceOf[HTMLInputElement]
          markRow(lastRow)
          selectContent(previousEditableCell(lastCell).get)
        }
      }
    } { prevCell =>
      // Select content of previous editable cell
      // Fires blur-callback (save) on current cell
      selectContent(prevCell)
      if (editCellId.isEmpty) {
        // Re-select content in original cell if invalid data
        selectContent(curCell)
      } else {
        markRow(curCell.parentNode)
      }
    }
    editCellId = ""
  }
}