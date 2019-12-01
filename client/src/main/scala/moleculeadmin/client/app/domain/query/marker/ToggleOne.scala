package moleculeadmin.client.app.domain.query.marker
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState.{dbSettingsIdOpt, _}
import moleculeadmin.client.app.domain.query.data.Indexes
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.{Element, HTMLCollection}
import org.scalajs.dom.window
import scala.concurrent.ExecutionContext.Implicits.global


case class ToggleOne(db: String, tableBody: TableSection, tpe: String)
  extends AppElements {

  type keepBooPickleImport_ToggleOne = PickleState

  val (curMarkerIndexes, onCls, offCls, iconIndex) = tpe match {
    case "star"  => (curStarIndexes, mark.starOn, mark.starOff, 0)
    case "flag"  => (curFlagIndexes, mark.flagOn, mark.flagOff, 1)
    case "check" => (curCheckIndexes, mark.checkOn, mark.checkOff, 2)
  }

  var cells: HTMLCollection = null
  var cell : Element        = null
  var i                     = 0
  var entityRow             = 0

  def toggle(eid: Long, isOn: Boolean): Unit = {
    val rows   = tableBody.children
    val eidStr = eid.toString
    val newCls = if (isOn) offCls else onCls

    // And set cur marker status
    val marked = if (isOn) {
      tpe match {
        case "star"  => curStars -= eid; "unstarred"
        case "flag"  => curFlags -= eid; "unflagged"
        case "check" => curChecks -= eid; "unchecked"
      }
    } else {
      tpe match {
        case "star"  => curStars += eid; "starred"
        case "flag"  => curFlags += eid; "flagged"
        case "check" => curChecks += eid; "checked"
      }
    }

    // Asynchronously save in meta db
    queryWire().toggleMarker(db, dbSettingsIdOpt, tpe, eid, isOn).call().foreach {
      case Left(err)            => window.alert(err)
      case Right(dbSettingsId1) =>
        dbSettingsIdOpt = Some(dbSettingsId1)
        println(s"Entity id $eid $marked")
    }

    def toggleIcon(tableCol: Int): Unit = {
      cell = cells(tableCol)
      if (cell.innerText == eidStr)
        cell.children(iconIndex).setAttribute("class", newCls)
    }

    def updateMarkerIndex(): Unit = {
      // Update marker in each entity column
      eidCols.foreach { tableCol =>

        // Eid might not be present in column
        val entityIndexOpt: Option[List[Int]] = curEntityIndexes(tableCol).get(eid)

        // Update other column only if it contains eid
        entityIndexOpt.foreach { entityIndex =>
          val markerIndex: Array[Boolean] = curMarkerIndexes(tableCol)

          i = 0
          while (i < entityIndex.length) {
            entityRow = entityIndex(i)
            markerIndex(entityRow) = !markerIndex(entityRow)
            i += 1
          }
        }
      }
    }

    eidCols.length match {
      case 1 => {
        val c1 = eidCols.head
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          toggleIcon(c1)
          i += 1
        }
        updateMarkerIndex()
      }

      case 2 => {
        val Seq(c1, c2) = eidCols
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          toggleIcon(c1)
          toggleIcon(c2)
          i += 1
        }
        updateMarkerIndex()
      }

      case 3 => {
        val Seq(c1, c2, c3) = eidCols
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          toggleIcon(c1)
          toggleIcon(c2)
          toggleIcon(c3)
          i += 1
        }
        updateMarkerIndex()
      }

      case 4 => {
        val Seq(c1, c2, c3, c4) = eidCols
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          toggleIcon(c1)
          toggleIcon(c2)
          toggleIcon(c3)
          toggleIcon(c4)
          i += 1
        }
        updateMarkerIndex()
      }

      case 5 => {
        val Seq(c1, c2, c3, c4, c5) = eidCols
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          toggleIcon(c1)
          toggleIcon(c2)
          toggleIcon(c3)
          toggleIcon(c4)
          toggleIcon(c5)
          i += 1
        }
        updateMarkerIndex()
      }

      case 6 => {
        val Seq(c1, c2, c3, c4, c5, c6) = eidCols
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          toggleIcon(c1)
          toggleIcon(c2)
          toggleIcon(c3)
          toggleIcon(c4)
          toggleIcon(c5)
          toggleIcon(c6)
          i += 1
        }
        updateMarkerIndex()
      }

      case 7 => {
        val Seq(c1, c2, c3, c4, c5, c6, c7) = eidCols
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          toggleIcon(c1)
          toggleIcon(c2)
          toggleIcon(c3)
          toggleIcon(c4)
          toggleIcon(c5)
          toggleIcon(c6)
          toggleIcon(c7)
          i += 1
        }
        updateMarkerIndex()
      }

      case n =>
        throw new IllegalArgumentException(
          "Can only mark up to 7 entity id columns. Found " + n
        )
    }
  }
}
