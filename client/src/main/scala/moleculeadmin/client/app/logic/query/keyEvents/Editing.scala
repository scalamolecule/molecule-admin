package moleculeadmin.client.app.logic.query.keyEvents

import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.app.logic.query.QueryState.{editCellId, groupEditId}
import org.scalajs.dom.html.TableRow
import org.scalajs.dom.raw.{HTMLInputElement, HTMLUListElement, KeyboardEvent}
import org.scalajs.dom.{Node, document, window}
import rx.Ctx
import scalatags.JsDom.all._


trait Editing extends Paging with AppElements {

  private def getColNo(id: String): Int =
    if (id.startsWith("grouped-cell-")) 1 else id.substring(4, 6).trim.toInt

  private def getFilterRow: TableRow = document.getElementById("tableHead")
    .lastChild.asInstanceOf[TableRow]

  private def getFirstRow: TableRow = document.getElementById("tableBody")
    .firstChild.asInstanceOf[TableRow]

  private def getLastRow: TableRow = document.getElementById("tableBody")
    .lastChild.asInstanceOf[TableRow]


  def selectContent(elem: Node): Unit = {
    val range = document.createRange()
    range.selectNodeContents(elem)
    val sel = window.getSelection()
    sel.removeAllRanges()
    sel.addRange(range)
  }

  def markNewRow(curRow: Node, newRow: Node): Unit = {
    markRow(curRow, "view")
    markRow(newRow)
  }

  def markRow(row: Node, status: String = "edit"): Unit = {
    row.asInstanceOf[TableRow].className = status
  }

  @scala.annotation.tailrec
  final private def previousEditableCell(testCell: HTMLInputElement): Option[HTMLInputElement] = {
    val prevCell = testCell.previousSibling.asInstanceOf[HTMLInputElement]
    if (prevCell == null)
      None
    else if (prevCell.isContentEditable)
      Some(prevCell)
    else
      previousEditableCell(prevCell)
  }

  @scala.annotation.tailrec
  final private def nextEditableCell(testCell: HTMLInputElement): Option[HTMLInputElement] = {
    val nextCell = testCell.nextSibling.asInstanceOf[HTMLInputElement]
    if (nextCell == null)
      None
    else if (nextCell.isContentEditable)
      Some(nextCell)
    else
      nextEditableCell(nextCell)
  }

  private def getFirstEditableCell(firstCellOnRow: HTMLInputElement): Option[HTMLInputElement] = {
    if (firstCellOnRow.isContentEditable) {
      Some(firstCellOnRow)
    } else {
      // Recurse forward to find first editable cell
      nextEditableCell(firstCellOnRow)
    }
  }

  private def getLastEditableCell(lastCellOnRow: HTMLInputElement): Option[HTMLInputElement] = {
    if (lastCellOnRow.isContentEditable) {
      Some(lastCellOnRow)
    } else {
      // Recurse backward to find last editable cell
      previousEditableCell(lastCellOnRow)
    }
  }

  private def headerCellToFirstCell(curCell: HTMLInputElement): Unit = {
    // Get col no from header `id="filter-23" contenteditable...`
    val colNo    = curCell.id.substring(7, 10).trim.replace("\"", "").toInt + 1
    val firstRow = getFirstRow
    if (firstRow != null) {
      val cellBelow: Node = firstRow.childNodes.item(colNo)
      selectContent(cellBelow)
      markRow(firstRow)
    }
  }


  // Editing -------------------------------------------------------------------

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
    val isCardManyOrMap = card != null && (card == "2" || card == "3")
    val cls             = curCell.getAttribute("class")
    val clss            = if (cls != null) cls.split(" ").toSeq else Seq.empty[String]
    val acceptMultiLine = clss.intersect(Seq("str", "input")).nonEmpty
    if (isCardManyOrMap || acceptMultiLine) {
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
        // Remove empty unordered list of empty cell
        curCell.removeChild(curCell.lastElementChild)
        // - Preserve soft line shifts (<br>)
        // - Decode control characters
        val curVs    = curCell.innerHTML.split("<br>").toList.flatMap(v =>
          Seq(_decode(List(v), "String").head: Frag, br)
        ).init
        // - Place text in unordered list
        val newUList = ul(li(curVs), li()).render
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


  // Cell navigation -----------------------------------------------------------

  private def up(e: KeyboardEvent, kind: Int)(implicit ctx: Ctx.Owner): Unit = {
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    editCellId = curCell.id
    val curRow   = curCell.parentNode
    val rowAbove = curRow.previousSibling

    if (curCell.classList.contains("header")) {
      // Ignore moving upwards from header cell

    } else if (rowAbove == null) {
      // Current row is first row
      if (isFirstPage) {
        // Trigger save
        curCell.blur()
        val filterCellAbove = getFilterRow.childNodes.item(getColNo(editCellId))
        selectContent(filterCellAbove)
        // Unmark edit row
        markRow(curRow, "")
      } else {
        // Go to previous page and select cell of same column on last row
        prevPage
        val lastRow   = getLastRow
        val cellAbove = lastRow.childNodes.item(getColNo(editCellId))
        selectContent(cellAbove)
        markRow(lastRow)
      }

    } else {
      // Select content of cell on above/first row
      val upRow     = kind match {
        case 1 => rowAbove
        case 2 => curRow.parentNode.firstChild
      }
      val cellAbove = upRow.childNodes.item(getColNo(editCellId))
      selectContent(cellAbove)
      if (editCellId.isEmpty) {
        // Re-select content in original cell if invalid data
        selectContent(curCell)
      } else {
        markNewRow(curRow, upRow)
      }
    }
    // reset current edit cell id
    editCellId = ""
  }

  private def down(e: KeyboardEvent, kind: Int)(implicit ctx: Ctx.Owner): Unit = {
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    editCellId = curCell.id
    val curRow   = curCell.parentNode
    val rowUnder = curRow.nextSibling

    if (curCell.classList.contains("groupEdit")) {
      // Trigger group edit spin and go to cell below
      groupEditId() = curCell.id
      headerCellToFirstCell(curCell)

    } else if (curCell.classList.contains("editable")) {
      // Go to cell below
      headerCellToFirstCell(curCell)

    } else if (curCell.classList.contains("header")) {
      // Ignore moving downwards in eid/t/tx/txInstant columns

    } else if (rowUnder == null) {
      // Last row
      if (isLastPage) {
        // On last page, save (blur) and re-select cell to not loose focus
        curCell.blur()
        selectContent(curCell)
        markRow(curRow)
      } else {
        // Go to next page and select cell of same column on first row
        nextPage
        val firstRow  = getFirstRow
        val cellBelow = firstRow.childNodes.item(getColNo(editCellId))
        selectContent(cellBelow)
        markRow(firstRow)
      }

    } else {
      // Rows below
      // Select content of cell on below/last row
      val downRow   = kind match {
        case 1 => rowUnder
        case 2 => curRow.parentNode.lastChild
      }
      val cellBelow = downRow.childNodes.item(getColNo(editCellId))
      // Select content of cell below
      // Fires blur-callback (save) on current cell
      selectContent(cellBelow)
      if (editCellId.isEmpty) {
        // Re-select content in original cell if invalid data
        selectContent(curCell)
      } else {
        markNewRow(curRow, downRow)
      }
    }
    // reset current edit cell id
    editCellId = ""
  }

  private def left(
    e: KeyboardEvent,
    curCell: HTMLInputElement,
    leftCellOpt: Option[HTMLInputElement]
  )(implicit ctx: Ctx.Owner): Unit = {
    e.preventDefault()
    editCellId = curCell.id
    val curRow = curCell.parentNode

    if (curCell.classList.contains("header")) {
      // Go to prev cell without triggering group edit
      editCellId = ""
      leftCellOpt.foreach(selectContent)

    } else {
      // Find first editable cell left of this cell
      leftCellOpt.fold {
        // Trigger update with blur
        curCell.blur()
        val rowAbove = curRow.previousSibling
        if (rowAbove == null) {
          // First row
          if (isFirstPage) {
            // Re-select cell to not loose focus
            selectContent(curCell)
            markRow(curRow)
          } else {
            // Go to previous page and select last editable cell on last row
            prevPage
            val lastRow          = getLastRow
            val lastCell         = lastRow.lastChild.asInstanceOf[HTMLInputElement]
            val lastEditableCell = getLastEditableCell(lastCell).get
            selectContent(lastEditableCell)
            markRow(lastRow)
          }
        } else {
          // Rows above
          // Go to last editable cell in previous row
          val lastCellPrevRow         = rowAbove.lastChild.asInstanceOf[HTMLInputElement]
          val lastEditableCellPrevRow = getLastEditableCell(lastCellPrevRow).get
          selectContent(lastEditableCellPrevRow)
          markNewRow(curRow, rowAbove)
        }
      } { leftCell =>
        // Select content of previous editable cell
        // Fires blur-callback (save) on current cell
        selectContent(leftCell)
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

  private def right(
    e: KeyboardEvent,
    curCell: HTMLInputElement,
    rightCellOpt: Option[HTMLInputElement]
  )(implicit ctx: Ctx.Owner): Unit = {
    e.preventDefault()
    editCellId = curCell.id
    val curRow = curCell.parentNode

    if (curCell.classList.contains("header")) {
      // Go to next cell without triggering group edit
      editCellId = ""
      rightCellOpt.foreach(selectContent)

    } else {
      // Find first editable cell right of cur cell
      rightCellOpt.fold {
        // Trigger update with blur
        curCell.blur()
        val rowUnder = curRow.nextSibling
        if (rowUnder == null) {
          // Last row
          if (isLastPage) {
            // Re-select cell to not loose focus
            selectContent(curCell)
            markRow(curRow)
          } else {
            // Go to next page and select first editable cell on first row
            nextPage
            val firstRow          = getFirstRow
            val firstCell         = firstRow.firstChild.asInstanceOf[HTMLInputElement]
            val firstEditableCell = getFirstEditableCell(firstCell).get
            selectContent(firstEditableCell)
            markRow(firstRow)
          }
        } else {
          // Rows below
          // Go to first editable cell in next row
          val firstCellNextRow         = rowUnder.firstChild.asInstanceOf[HTMLInputElement]
          val firstEditableCellNextRow = getFirstEditableCell(firstCellNextRow).get
          selectContent(firstEditableCellNextRow)
          markNewRow(curRow, rowUnder)
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


  // Adjacent cell movements ---------------------------------------------------

  def cellUp(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = up(e, 1)

  def cellDown(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = down(e, 1)

  def cellLeft(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    val curCell     = document.activeElement.asInstanceOf[HTMLInputElement]
    val leftOptCell = previousEditableCell(curCell)
    left(e, curCell, leftOptCell)
  }

  def cellRight(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    val curCell      = document.activeElement.asInstanceOf[HTMLInputElement]
    val rightOptCell = nextEditableCell(curCell)
    right(e, curCell, rightOptCell)
  }

  // Page movements ------------------------------------------------------------

  def pageUp(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = up(e, 2)

  def pageDown(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = down(e, 2)

  def startOfRow(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    val curCell      = document.activeElement.asInstanceOf[HTMLInputElement]
    val firstCell    = curCell.parentNode.firstChild.asInstanceOf[HTMLInputElement]
    val firstOptCell = getFirstEditableCell(firstCell)
    left(e, curCell, firstOptCell)
  }

  def endOfRow(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    val curCell     = document.activeElement.asInstanceOf[HTMLInputElement]
    val lastCell    = curCell.parentNode.lastChild.asInstanceOf[HTMLInputElement]
    val lastOptCell = getLastEditableCell(lastCell)
    right(e, curCell, lastOptCell)
  }


  // First/last page -----------------------------------------------------------

  def first(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    if (!isFirstPage) {
      firstPage
    }
    val firstRow  = getFirstRow
    val firstCell = firstRow.childNodes.item(getColNo(curCell.id))
    selectContent(firstCell)
    markRow(firstRow)
    editCellId = ""
  }

  def last(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    e.preventDefault()
    val curCell = document.activeElement.asInstanceOf[HTMLInputElement]
    if (!isLastPage) {
      lastPage
    }
    val lastRow  = getLastRow
    val lastCell = lastRow.childNodes.item(getColNo(curCell.id))
    selectContent(lastCell)
    markRow(lastRow)
    editCellId = ""
  }
}