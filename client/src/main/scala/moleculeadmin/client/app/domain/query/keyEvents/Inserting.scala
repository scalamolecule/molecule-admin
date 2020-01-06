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
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.BaseQuery
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{TableCell, TableSection}
import org.scalajs.dom.raw.{Element, KeyboardEvent}
import org.scalajs.dom.{document, window}
import rx.Ctx
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global

trait Inserting extends BaseKeyEvents with BaseQuery with BodyElements with RegexMatching
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


  private def extract(col: Col, html: String): Seq[String] = {
    val (attr, attrType, card, enums, mandatory) =
      (col.attr, col.attrType, col.card, col.enums, !col.opt)

    def err(msg: String) = {
      window.alert(msg)
      throw new IllegalArgumentException(msg)
    }

    def validate(str: String): String = {
      if (!valid(attrType, str)) {
        err(s"Invalid `$attr` value of type `$attrType`:\n$str")
      } else {
        str
      }
    }

    card match {
      case 1 =>
        val v = _html2str(html)
        if (v.nonEmpty) {
          Seq(validate(v))
        } else if (mandatory) {
          err(s"Mandatory `$attr` value can't be empty")
        } else {
          Nil
        }

      case 2 =>
        val strs = if (attrType == "ref") {
          val s1 = html.replaceAll("</*ul[^>]*>", "")
          s1.substring(s1.indexOf(">", 5) + 1, s1.length - 5)
            .replaceAll("(<br>)*$", "")
            .split("""</li><li class="eid(Chosen)*">""").toList
        } else {
          html.substring(8, html.length - 10).split("</li><li>").toList
        }
        val vs   = if (attrType == "String" && enums.isEmpty) {
          strs.map(_
            .replaceAllLiterally("&nbsp;", " ")
            .replaceAllLiterally("&lt;", "<")
            .replaceAllLiterally("&gt;", ">")
            .replaceAllLiterally("&amp;", "&")
            .replaceAllLiterally("<br>", "\n")
            .trim)
            .filter(_.nonEmpty)
            .map(_html2str)
        } else {
          strs.flatMap(
            _.split("<br>")
              .map(_
                .replaceAllLiterally("&nbsp;", " ")
                .replaceAllLiterally("&lt;", "<")
                .replaceAllLiterally("&gt;", ">")
                .replaceAllLiterally("&amp;", "&")
                .trim)
              .filter(_.nonEmpty)
          )
        }
        if (vs.nonEmpty) {
          vs.distinct.map(validate)
        } else if (mandatory) {
          err(s"Mandatory `$attr` value can't be empty")
        } else {
          Nil
        }

      case 3 =>
        val strs = html
          .substring(8, html.length - 10)
          .split("</li><li>")
          .toList

        val vs = if (attrType == "String" && enums.isEmpty) {
          strs
            .map(_
              .replaceAllLiterally("&nbsp;", " ")
              .replaceAllLiterally("&lt;", "<")
              .replaceAllLiterally("&gt;", ">")
              .replaceAllLiterally("&amp;", "&")
              .replaceAllLiterally("<br>", "\n")
              .trim)
            .filter(_.nonEmpty)
            .map(_html2str)
        } else {
          strs.flatMap(
            _.split("<br>")
              .map(_
                .replaceAllLiterally("&nbsp;", " ")
                .replaceAllLiterally("&lt;", "<")
                .replaceAllLiterally("&gt;", ">")
                .replaceAllLiterally("&amp;", "&")
                .trim)
              .filter(_.nonEmpty)
          )
        }

        if (vs.nonEmpty) {
          vs.map { pair =>
            if (!pair.contains("->")) {
              err("Key/value should be separated by '->'")
            }
            val k = pair.substring(0, pair.indexOf("->")).trim
            val v = pair.substring(pair.indexOf("->") + 2).trim
            if (attrType == "Date")
              k + "__~~__" + truncateDateStr(validate(v))
            else
              k + "__~~__" + validate(v)
          }
        } else if (mandatory) {
          err(s"Mandatory `$attr` value can't be empty")
        } else {
          Nil
        }
    }
  }

  def insertNewRow(e: KeyboardEvent)(implicit ctx: Ctx.Owner): Unit = {
    // prevent creating new line within cell
    e.preventDefault()

    val row       = document.activeElement.parentNode
    val tableBody = row.parentNode.asInstanceOf[TableSection]
    val cells     = row.childNodes
    val rowValues = columns.now.tail.map(col =>
      extract(col, cells.item(col.colIndex + 1).asInstanceOf[TableCell].innerHTML)
    )
    if (rowValues.forall(_.isEmpty)) {
      val err = s"All values can't be empty"
      window.alert(err)
      return
    }

    val eidCell = cells.item(1).asInstanceOf[TableCell]
    eidCell.innerText = ""

    queryWire().insert(db, curMolecule.now, nsMap, rowValues).call().foreach {
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
    val newCells  = columns.now.tail.map {
      case col if col.card == 1 =>
        if (isNumber(col.attrType))
          td(contenteditable := true, textAlign.right)
        else
          td(contenteditable := true)

      case _ =>
        td(
          cls := "items",
          attr("card") := 2,
          contenteditable := true,
          ul(li())
        )
    }

    val newRow = tr(
      td(), // (n)
      td(textAlign.right, "New entity data --> "),
      newCells
    ).render
    tableBody.appendChild(newRow)
    newRow.children.item(2).asInstanceOf[TableCell].focus()
  }
}
