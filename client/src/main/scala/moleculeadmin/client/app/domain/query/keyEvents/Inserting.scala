package moleculeadmin.client.app.domain.query.keyEvents

import moleculeadmin.client.app.domain.query.QueryState._
import org.scalajs.dom.html.TableCell
import org.scalajs.dom.raw.{Element, KeyboardEvent}
import org.scalajs.dom.{document, window}
import rx.Ctx


trait Inserting {

  protected var insertMode = false

  def addInsertNewDataRow0(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit ={
    // Don't write "n" in new cell
    e.preventDefault()
    addInsertNewDataRow()
  }

  def addInsertNewDataRow()(implicit ctx: Ctx.Owner): Unit = {
    if (groupableCols.isEmpty) {
      window.alert("Add `e` first to allow inserting new data")
      return
    }

    insertMode = true
    val tableBody = document.getElementById("tableBody")
    val rows      = tableBody.children
    val rowCount  = rows.length
    val lastRow   = rows.item(rowCount - 1)
    val newRow    = lastRow.cloneNode(true).asInstanceOf[Element]
    val cells     = newRow.children

    0 until cells.length foreach {
      case 0 => cells.item(0).innerText = ""
      case 1 => cells.item(1).innerText = "New entity data --> "
      case i =>
        val cell = cells.item(i)
        cell.innerText = ""
        cell.setAttribute("contenteditable", "true")
    }
    tableBody.appendChild(newRow)
    cells.item(2).asInstanceOf[TableCell].focus()
  }

  def insertNewRow(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {

    // prevent creating new line within cell
    e.preventDefault()
  }
}
