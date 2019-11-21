package moleculeadmin.client.app.domain.query.marker
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState.{dbSettingsIdOpt, _}
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.{Element, HTMLCollection}
import org.scalajs.dom.window
import scala.concurrent.ExecutionContext.Implicits.global


case class ToggleOne(db: String, tableBody: TableSection, tpe: String)
  extends AppElements {

  type keepBooPickleImport = PickleState

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
    val rows = tableBody.children
    val eidStr = eid.toString
    val newCls = if (isOn) {
      tpe match {
        case "star"  => curStars -= eid
        case "flag"  => curFlags -= eid
        case "check" => curChecks -= eid
      }
      offCls
    } else {
      tpe match {
        case "star"  => curStars += eid
        case "flag"  => curFlags += eid
        case "check" => curChecks += eid
      }
      onCls
    }

    queryWire().toggleMarker(db, dbSettingsIdOpt, tpe, eid, isOn).call().foreach {
      case Left(err)            => window.alert(err)
      case Right(dbSettingsId1) =>
        dbSettingsIdOpt = Some(dbSettingsId1)
        println(s"Eid $eid $tpe: " + !isOn)
    }


    def setMarkerClass(tableCol: Int): Unit = {
      cell = cells(tableCol)
      if (cell.innerText == eidStr)
        cell.children(iconIndex).setAttribute("class", newCls)
    }

    def updateMarkerIndex(tableCol: Int) = {
      // Eid might not be present in other column
      val entityIndexOpt = curEntityIndexes(tableCol).get(eid)
      val markerIndex    = curMarkerIndexes(tableCol)
      entityIndexOpt.foreach { entityIndex =>
        i = 0
        while (i < entityIndex.length) {
          entityRow = entityIndex(i)
          markerIndex(entityRow) = !markerIndex(entityRow)
          i += 1
        }
      }
    }

    eTableColIndexes.length match {
      case 1 => {
        val c1 = eTableColIndexes.head
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass(c1)
          i += 1
        }
        updateMarkerIndex(c1)
      }

      case 2 => {
        val Seq(c1, c2) = eTableColIndexes

        // Update visible rows
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass(c1)
          setMarkerClass(c2)
          i += 1
        }
        updateMarkerIndex(c1)
        updateMarkerIndex(c2)
      }

      case 3 => {
        val Seq(c1, c2, c3) = eTableColIndexes
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass(c1)
          setMarkerClass(c2)
          setMarkerClass(c3)
          i += 1
        }
        updateMarkerIndex(c1)
        updateMarkerIndex(c2)
        updateMarkerIndex(c3)
      }

      case 4 => {
        val Seq(c1, c2, c3, c4) = eTableColIndexes
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass(c1)
          setMarkerClass(c2)
          setMarkerClass(c3)
          setMarkerClass(c4)
          i += 1
        }
        updateMarkerIndex(c1)
        updateMarkerIndex(c2)
        updateMarkerIndex(c3)
        updateMarkerIndex(c4)
      }

      case 5 => {
        val Seq(c1, c2, c3, c4, c5) = eTableColIndexes
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass(c1)
          setMarkerClass(c2)
          setMarkerClass(c3)
          setMarkerClass(c4)
          setMarkerClass(c5)
          i += 1
        }
        updateMarkerIndex(c1)
        updateMarkerIndex(c2)
        updateMarkerIndex(c3)
        updateMarkerIndex(c4)
        updateMarkerIndex(c5)
      }

      case 6 => {
        val Seq(c1, c2, c3, c4, c5, c6) = eTableColIndexes
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass(c1)
          setMarkerClass(c2)
          setMarkerClass(c3)
          setMarkerClass(c4)
          setMarkerClass(c5)
          setMarkerClass(c6)
          i += 1
        }
        updateMarkerIndex(c1)
        updateMarkerIndex(c2)
        updateMarkerIndex(c3)
        updateMarkerIndex(c4)
        updateMarkerIndex(c5)
        updateMarkerIndex(c6)
      }

      case 7 => {
        val Seq(c1, c2, c3, c4, c5, c6, c7) = eTableColIndexes
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass(c1)
          setMarkerClass(c2)
          setMarkerClass(c3)
          setMarkerClass(c4)
          setMarkerClass(c5)
          setMarkerClass(c6)
          setMarkerClass(c7)
          i += 1
        }
        updateMarkerIndex(c1)
        updateMarkerIndex(c2)
        updateMarkerIndex(c3)
        updateMarkerIndex(c4)
        updateMarkerIndex(c5)
        updateMarkerIndex(c6)
        updateMarkerIndex(c7)
      }

      case n =>
        throw new IllegalArgumentException(
          "Can only mark up to 7 entity id columns. Found " + n
        )
    }
  }
}
