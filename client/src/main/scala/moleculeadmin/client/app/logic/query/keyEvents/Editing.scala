package moleculeadmin.client.app.logic.query.keyEvents

import moleculeadmin.client.app.logic.query.QueryState.{editCellId, groupEditId}
import org.scalajs.dom.html.TableRow
import org.scalajs.dom.raw.{HTMLInputElement, HTMLUListElement, KeyboardEvent}
import org.scalajs.dom.{Node, document, window}
import rx.Ctx
import scalatags.JsDom.all._


trait Editing extends Paging {

  def getColNo(id: String): Int =
    if (id.startsWith("grouped-cell-")) 1 else id.substring(4, 6).trim.toInt

  def getFirstRow: TableRow = document.getElementById("tableBody")
    .firstChild.asInstanceOf[TableRow]

  def getLastRow: TableRow = document.getElementById("tableBody")
    .lastChild.asInstanceOf[TableRow]

  def selectContent(elem: Node): Unit = {
    val range = document.createRange()
    range.selectNodeContents(elem)
    val sel = window.getSelection()
    sel.removeAllRanges()
    sel.addRange(range)
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

  def saveEditMoveUp(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent creating new line within cell
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    editCellId = curCell.id
    val curRow   = curCell.parentNode
    val rowAbove = curRow.previousSibling
    if (curCell.classList.contains("header")) {
      // Ignore moving upwards from header cell
    } else if (rowAbove != null) {
      val cellAbove = rowAbove.childNodes.item(getColNo(editCellId))
      // Select content of cell above
      // Fires blur-callback (save) on current cell
      selectContent(cellAbove)
      if (editCellId.isEmpty) {
        // Re-select content in original cell if invalid data
        selectContent(curCell)
      } else {
        markNewRow(curRow, rowAbove)
      }
    } else {
      // Current row is first row
      if (isFirstPage) {
        // On first page, re-select cell to not loose focus
        curCell.blur()
        selectContent(curCell)
        markRow(curRow)
      } else {
        // Go to previous page and select cell of same column on last row
        prevPage
        val lastRow   = getLastRow
        val cellAbove = lastRow.childNodes.item(getColNo(editCellId))
        markRow(lastRow)
        selectContent(cellAbove)
      }
    }
    // reset current edit cell id
    editCellId = ""
  }

  def saveEditMoveDown(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent creating new line within cell
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    editCellId = curCell.id
    val curRow   = curCell.parentNode
    val rowUnder = curRow.nextSibling

    if (curCell.classList.contains("header")) {
      // Trigger group edit calculation (blur current cell) and stay in cell
      curCell.blur()
      groupEditId() = curCell.id
      selectContent(curCell)

    } else if (rowUnder != null) {
      val cellBelow = rowUnder.childNodes.item(getColNo(editCellId))
      // Select content of cell below
      // Fires blur-callback (save) on current cell
      selectContent(cellBelow)
      if (editCellId.isEmpty) {
        // Re-select content in original cell if invalid data
        selectContent(curCell)
      } else {
        markNewRow(curRow, rowUnder)
      }
    } else {
      // Current row is last row
      if (isLastPage) {
        // On last page, save (blur) and re-select cell to not loose focus
        curCell.blur()
        selectContent(curCell)
        markRow(curRow)
      } else {
        // Go to next page and select cell of same column on first row
        nextPage
        val firstRow = getFirstRow
        markRow(firstRow)
        val cellBelow = firstRow.childNodes.item(getColNo(editCellId))
        selectContent(cellBelow)
      }
    }
    // reset current edit cell id
    editCellId = ""
  }

  def saveEditMoveForward(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent immediately moving to next cell
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    editCellId = curCell.id

    @scala.annotation.tailrec
    def nextEditableCell(testCell: HTMLInputElement): Option[HTMLInputElement] = {
      val nextCell = testCell.nextSibling.asInstanceOf[HTMLInputElement]
      if (nextCell == null)
        None
      else if (nextCell.isContentEditable)
        Some(nextCell)
      else
        nextEditableCell(nextCell)
    }

    if (curCell.classList.contains("header")) {
      // Go to next cell without triggering group edit
      editCellId = ""
      nextEditableCell(curCell).foreach(selectContent)
    } else {
      nextEditableCell(curCell).fold {
        // Trigger update with blur
        curCell.blur()
        val curRow   = curCell.parentNode
        val rowUnder = curRow.nextSibling
        if (rowUnder != null) {
          // Go to first selectable cell in next row
          val firstCellNextRow = rowUnder.firstChild.asInstanceOf[HTMLInputElement]
          selectContent(nextEditableCell(firstCellNextRow).get)
          markNewRow(curRow, rowUnder)
        } else {
          if (isLastPage) {
            // Re-select cell to not loose focus
            selectContent(curCell)
            markRow(curRow)
          } else {
            // Go to next page and select first editable cell on first row
            nextPage
            val firstRow  = getFirstRow
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
  }


  def saveEditMoveBackwards(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent immediately moving to next cell
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    editCellId = curCell.id

    @scala.annotation.tailrec
    def previousEditableCell(testCell: HTMLInputElement): Option[HTMLInputElement] = {
      val prevCell = testCell.previousSibling.asInstanceOf[HTMLInputElement]
      if (prevCell == null)
        None
      else if (prevCell.isContentEditable)
        Some(prevCell)
      else
        previousEditableCell(prevCell)
    }

    if (curCell.classList.contains("header")) {
      // Go to prev cell without triggering group edit
      editCellId = ""
      previousEditableCell(curCell).foreach(selectContent)
    } else {
      previousEditableCell(curCell).fold {
        // Trigger update with blur
        curCell.blur()
        val curRow   = curCell.parentNode
        val rowAbove = curRow.previousSibling
        if (rowAbove != null) {
          // Go to last selectable cell in previous row
          val lastCellPrevRow = rowAbove.lastChild.asInstanceOf[HTMLInputElement]
          selectContent(previousEditableCell(lastCellPrevRow).get)
          markNewRow(curRow, rowAbove)
        } else {
          if (isFirstPage) {
            // Re-select cell to not loose focus
            selectContent(curCell)
            markRow(curRow)
          } else {
            // Go to previous page and select last editable cell on last row
            prevPage
            val lastRow  = getLastRow
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
}