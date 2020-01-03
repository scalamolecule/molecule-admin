package moleculeadmin.client.app.domain.query.keyEvents
import autowire._
import boopickle.Default._
import molecule.util.RegexMatching
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.TypeValidation
import moleculeadmin.client.app.domain.query.data.edit.RetractEid
import moleculeadmin.client.app.domain.query.marker.ToggleOne
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{TableCell, TableSection}
import org.scalajs.dom.raw.{Element, KeyboardEvent}
import org.scalajs.dom.{document, window}
import rx.Ctx
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global

trait Inserting extends Base with BodyElements with RegexMatching
  with AppElements with TypeValidation {
  type keepBooPickleImport_Inserting = PickleState

  protected var insertMode = false

  def addInsertNewDataRow0(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
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
    prepareEmptyInsertRow()
  }

  def insertNewRow(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent creating new line within cell
    e.preventDefault()

    val row       = document.activeElement.parentNode
    val tableBody = row.parentNode.asInstanceOf[TableSection]
    val cells     = row.childNodes

    val data = columns.now.tail.map { col =>
      val (colIndex, colType, attr, attrType, card) =
        (col.colIndex, col.colType, col.attr, col.attrType, col.card)

      val str = _html2str(cells.item(colIndex + 1).asInstanceOf[TableCell].innerHTML)

      if (!valid(attrType, str)) {
        window.alert(s"Invalid `$attr` value of type `$attrType`:\n$str")
        return
      }
      //      println(s"$attr: " + str)
      str
    }

    val eidCell = cells.item(1).asInstanceOf[TableCell]
    eidCell.innerText = ""

    queryWire().insert(db, curMolecule.now, nsMap, data).call().foreach {
      case Right(eid) =>
        // Show created eid and mark it as starred
        eidCell.setAttribute("style", "color: " + Color.textDarkGray).render
        eidCell.appendChild(_xRetract(() => RetractEid(eid)).render)
        eidCell.appendChild(eid.render)
        eidCell.appendChild(i(cls := mark.starOff).render)
        eidCell.appendChild(i(cls := mark.flagOff).render)
        eidCell.appendChild(i(cls := mark.checkOff).render)
        ToggleOne(tableBody, "star").toggle(eid, false)

      case Left(err) =>
        val msg     = "Error inserting previous row: " + err
        val rows    = tableBody.children
        val lastRow = rows.item(rows.length - 1)
        tableBody.removeChild(lastRow)
        val remainingRows = tableBody.children
        val prevRow       = remainingRows.item(rows.length - 1)
        prevRow.children.item(2).asInstanceOf[TableCell].focus()
        println(msg)
        window.alert(msg)
    }
    prepareEmptyInsertRow()
  }

  def abortInsert(): Unit = {
    insertMode = false
    modelElements.recalc()
  }


  def prepareEmptyInsertRow(): Unit = {
    val tableBody = document.getElementById("tableBody")
    val rows      = tableBody.children
    val prevRow   = rows.item(rows.length - 1)
    val newRow    = prevRow.cloneNode(true).asInstanceOf[Element]
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
}
