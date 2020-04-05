package moleculeadmin.client.app.logic.query.keyEvents
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.edit.{Insert, RetractEid}
import moleculeadmin.client.app.logic.query.marker.Toggle
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.BaseQuery
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{TableCell, TableSection}
import org.scalajs.dom.raw.{HTMLCollection, KeyboardEvent}
import org.scalajs.dom.{document, window}
import rx.Ctx
import scalatags.JsDom.all.{onblur, _}
import scala.concurrent.ExecutionContext.Implicits.global

trait Inserting extends Insert with BaseQuery with Editing {

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
    val rowValues = editableCols.map { col =>
      extract(col, cells.item(col.colIndex + 1).asInstanceOf[TableCell])
    }
    if (rowValues.forall(_.isEmpty)) {
      val err = s"All values can't be empty"
      window.alert(err)
      return
    }

    val eidCell = cells.item(1).asInstanceOf[TableCell]
    eidCell.innerText = ""

    queryWireAjax().insert(db, curMolecule.now, nsMap, rowValues).call().foreach {
      case Right(eid) =>
        // Render eid cell and toggle star
        eidCell.setAttribute("style", "color: " + Color.textDarkGray).render
        eidCell.appendChild(_xRetract(() => RetractEid(eid)).render)
        eidCell.appendChild(eid.render)
        eidCell.appendChild(i(cls := mark.starOff).render)
        eidCell.appendChild(i(cls := mark.flagOff).render)
        eidCell.appendChild(i(cls := mark.checkOff).render)
        Toggle(tableBody, "star", false, eid = eid)


      // Show inserted data
        println(s"Inserted entity $eid:\n  " +
          valuePairs(rowValues).mkString("\n  "))

      case Left(err) =>
        correctPrevRow(tableBody, "Error inserting previous row: " + err)
    }
    keepInserting = true
    prepareEmptyInsertRow()
  }

  def abortInsert(): Unit = {
    insertMode = false
    modelElements.recalc()
  }

  private def removeInsertRow(tableBody: TableSection): HTMLCollection = {
    val rows    = tableBody.children
    val lastRow = rows.item(rows.length - 1)
    tableBody.removeChild(lastRow)
    rows
  }

  private def correctPrevRow(tableBody: TableSection, err: String = ""): Unit = {
    val rows          = removeInsertRow(tableBody)
    val remainingRows = tableBody.children
    val prevRow       = remainingRows.item(rows.length - 1)
    prevRow.children.item(2).asInstanceOf[TableCell].focus()
    if (err.nonEmpty) {
      println(err)
      window.alert(err)
    }
  }

  def continueInserting(): Unit = {
    // Keep inserting when tabbing
    keepInserting = true
  }

  def prepareEmptyInsertRow(): Unit = {
    val tableBody = document.getElementById("tableBody").asInstanceOf[TableSection]
    val newCells  = columns.now.tail.map {
      case col if col.attr == "e"        => td()
      case Col(_, _, _, _, _, _, _, _, _, _, _,
      "t" | "tx" | "txInstant", _, _, _) => td()

      case col =>
        val newCell = td(
          contenteditable := true,
          onblur := { () =>
            if (error) {
              // Correct invalid input
              error = false
            } else if (keepInserting) {
              // Next blur should be an abort unless tabbing
              keepInserting = false
            } else {
              keepInserting = false
              insertMode = false
              modelElements.recalc()
            }
          },
        )
        col.card match {
          case 1 =>
            if (isNumber(col.attrType))
              newCell(textAlign.right)
            else
              newCell

          case card =>
            if (col.attrType == "String")
              newCell(attr("card") := card, cls := "items", ul(li()))
            else if (isNumber(col.attrType))
              newCell(attr("card") := card,
                if (card == 2) cls := "num" else ()
              )
            else
              newCell(attr("card") := card)
        }
    }

    val newRow = tr(
      td(), // (n)
      td(textAlign.right, "New entity data --> "),
      newCells
    ).render
    tableBody.appendChild(newRow)
    newRow.children.item(2).asInstanceOf[TableCell].focus()
    markNewRow(tableBody.lastElementChild, newRow)
  }
}
