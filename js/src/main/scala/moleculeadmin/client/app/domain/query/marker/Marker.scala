package moleculeadmin.client.app.domain.query.marker
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.AppElements
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.{Element, HTMLCollection}

case class Marker(
  tableBody: TableSection,
  eColIndexes: Seq[Int],
  tpe: String
) extends AppElements {

  var (curMarkerIndexes, curMarker, onCls, offCls, iconIndex) = tpe match {
    case "star"  => (curStarIndexes, curStars, mark.starOn, mark.starOff, 0)
    case "flag"  => (curFlagIndexes, curFlags, mark.flagOn, mark.flagOff, 1)
    case "check" => (curCheckIndexes, curChecks, mark.checkOn, mark.checkOff, 2)
  }

  // Placeholders to avoid variable allocations
  var cells: HTMLCollection = null
  var cell : Element        = null

  def toggle(eid: Long, isOn: Boolean): Unit = {
    val eidStr = eid.toString
    val newCls = if (isOn) {
      curMarker = curMarker.filterNot(_ == eid)
      offCls
    } else {
      curMarker = eid :: curMarker
      onCls
    }

    val rows      = tableBody.children
    var i         = 0
    var entityRow = 0

    eColIndexes.length match {
      case 1 => {
        val c1 = eColIndexes.head
        while (i < rows.length) {
          cells = rows(i).children
          cell = cells(c1)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          i += 1
        }

        val entityIndex1 = curEntityIndexes(c1)(eid)
        val markerIndex1 = curMarkerIndexes(c1)

        i = 0
        while (i < entityIndex1.length) {
          entityRow = entityIndex1(i)
          markerIndex1(entityRow) = !markerIndex1(entityRow)
          i += 1
        }
      }

      case 2 => {
        val Seq(c1, c2) = eColIndexes
        while (i < rows.length) {
          cells = rows(i).children
          cell = cells(c1)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c2)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          i += 1
        }

        val entityIndex1 = curEntityIndexes(c1)(eid)
        val entityIndex2 = curEntityIndexes(c2)(eid)
        val markerIndex1 = curMarkerIndexes(c1)
        val markerIndex2 = curMarkerIndexes(c2)

        i = 0
        while (i < entityIndex1.length) {
          entityRow = entityIndex1(i)
          markerIndex1(entityRow) = !markerIndex1(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex2.length) {
          entityRow = entityIndex2(i)
          markerIndex2(entityRow) = !markerIndex2(entityRow)
          i += 1
        }
      }

      case 3 => {
        val Seq(c1, c2, c3) = eColIndexes
        while (i < rows.length) {
          cells = rows(i).children
          cell = cells(c1)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c2)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c3)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          i += 1
        }

        val entityIndex1 = curEntityIndexes(c1)(eid)
        val entityIndex2 = curEntityIndexes(c2)(eid)
        val entityIndex3 = curEntityIndexes(c3)(eid)
        val markerIndex1 = curMarkerIndexes(c1)
        val markerIndex2 = curMarkerIndexes(c2)
        val markerIndex3 = curMarkerIndexes(c3)

        i = 0
        while (i < entityIndex1.length) {
          entityRow = entityIndex1(i)
          markerIndex1(entityRow) = !markerIndex1(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex2.length) {
          entityRow = entityIndex2(i)
          markerIndex2(entityRow) = !markerIndex2(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex3.length) {
          entityRow = entityIndex3(i)
          markerIndex3(entityRow) = !markerIndex3(entityRow)
          i += 1
        }
      }

      case 4 => {
        val Seq(c1, c2, c3, c4) = eColIndexes
        while (i < rows.length) {
          cells = rows(i).children
          cell = cells(c1)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c2)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c3)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c4)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          i += 1
        }

        val entityIndex1 = curEntityIndexes(c1)(eid)
        val entityIndex2 = curEntityIndexes(c2)(eid)
        val entityIndex3 = curEntityIndexes(c3)(eid)
        val entityIndex4 = curEntityIndexes(c4)(eid)
        val markerIndex1 = curMarkerIndexes(c1)
        val markerIndex2 = curMarkerIndexes(c2)
        val markerIndex3 = curMarkerIndexes(c3)
        val markerIndex4 = curMarkerIndexes(c4)

        i = 0
        while (i < entityIndex1.length) {
          entityRow = entityIndex1(i)
          markerIndex1(entityRow) = !markerIndex1(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex2.length) {
          entityRow = entityIndex2(i)
          markerIndex2(entityRow) = !markerIndex2(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex3.length) {
          entityRow = entityIndex3(i)
          markerIndex3(entityRow) = !markerIndex3(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex4.length) {
          entityRow = entityIndex4(i)
          markerIndex4(entityRow) = !markerIndex4(entityRow)
          i += 1
        }
      }

      case 5 => {
        val Seq(c1, c2, c3, c4, c5) = eColIndexes
        while (i < rows.length) {
          cells = rows(i).children
          cell = cells(c1)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c2)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c3)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c4)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c5)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          i += 1
        }

        val entityIndex1 = curEntityIndexes(c1)(eid)
        val entityIndex2 = curEntityIndexes(c2)(eid)
        val entityIndex3 = curEntityIndexes(c3)(eid)
        val entityIndex4 = curEntityIndexes(c4)(eid)
        val entityIndex5 = curEntityIndexes(c5)(eid)
        val markerIndex1 = curMarkerIndexes(c1)
        val markerIndex2 = curMarkerIndexes(c2)
        val markerIndex3 = curMarkerIndexes(c3)
        val markerIndex4 = curMarkerIndexes(c4)
        val markerIndex5 = curMarkerIndexes(c5)

        i = 0
        while (i < entityIndex1.length) {
          entityRow = entityIndex1(i)
          markerIndex1(entityRow) = !markerIndex1(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex2.length) {
          entityRow = entityIndex2(i)
          markerIndex2(entityRow) = !markerIndex2(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex3.length) {
          entityRow = entityIndex3(i)
          markerIndex3(entityRow) = !markerIndex3(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex4.length) {
          entityRow = entityIndex4(i)
          markerIndex4(entityRow) = !markerIndex4(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex5.length) {
          entityRow = entityIndex5(i)
          markerIndex5(entityRow) = !markerIndex5(entityRow)
          i += 1
        }
      }

      case 6 => {
        val Seq(c1, c2, c3, c4, c5, c6) = eColIndexes
        while (i < rows.length) {
          cells = rows(i).children
          cell = cells(c1)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c2)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c3)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c4)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c5)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c6)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          i += 1
        }

        val entityIndex1 = curEntityIndexes(c1)(eid)
        val entityIndex2 = curEntityIndexes(c2)(eid)
        val entityIndex3 = curEntityIndexes(c3)(eid)
        val entityIndex4 = curEntityIndexes(c4)(eid)
        val entityIndex5 = curEntityIndexes(c5)(eid)
        val entityIndex6 = curEntityIndexes(c6)(eid)
        val markerIndex1 = curMarkerIndexes(c1)
        val markerIndex2 = curMarkerIndexes(c2)
        val markerIndex3 = curMarkerIndexes(c3)
        val markerIndex4 = curMarkerIndexes(c4)
        val markerIndex5 = curMarkerIndexes(c5)
        val markerIndex6 = curMarkerIndexes(c6)

        i = 0
        while (i < entityIndex1.length) {
          entityRow = entityIndex1(i)
          markerIndex1(entityRow) = !markerIndex1(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex2.length) {
          entityRow = entityIndex2(i)
          markerIndex2(entityRow) = !markerIndex2(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex3.length) {
          entityRow = entityIndex3(i)
          markerIndex3(entityRow) = !markerIndex3(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex4.length) {
          entityRow = entityIndex4(i)
          markerIndex4(entityRow) = !markerIndex4(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex5.length) {
          entityRow = entityIndex5(i)
          markerIndex5(entityRow) = !markerIndex5(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex6.length) {
          entityRow = entityIndex6(i)
          markerIndex6(entityRow) = !markerIndex6(entityRow)
          i += 1
        }
      }

      case 7 => {
        val Seq(c1, c2, c3, c4, c5, c6, c7) = eColIndexes
        while (i < rows.length) {
          cells = rows(i).children
          cell = cells(c1)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c2)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c3)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c4)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c5)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c6)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          cell = cells(c7)
          if (cell.innerText == eidStr)
            cell.children(iconIndex).setAttribute("class", newCls)

          i += 1
        }

        val entityIndex1 = curEntityIndexes(c1)(eid)
        val entityIndex2 = curEntityIndexes(c2)(eid)
        val entityIndex3 = curEntityIndexes(c3)(eid)
        val entityIndex4 = curEntityIndexes(c4)(eid)
        val entityIndex5 = curEntityIndexes(c5)(eid)
        val entityIndex6 = curEntityIndexes(c6)(eid)
        val entityIndex7 = curEntityIndexes(c7)(eid)
        val markerIndex1 = curMarkerIndexes(c1)
        val markerIndex2 = curMarkerIndexes(c2)
        val markerIndex3 = curMarkerIndexes(c3)
        val markerIndex4 = curMarkerIndexes(c4)
        val markerIndex5 = curMarkerIndexes(c5)
        val markerIndex6 = curMarkerIndexes(c6)
        val markerIndex7 = curMarkerIndexes(c7)

        i = 0
        while (i < entityIndex1.length) {
          entityRow = entityIndex1(i)
          markerIndex1(entityRow) = !markerIndex1(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex2.length) {
          entityRow = entityIndex2(i)
          markerIndex2(entityRow) = !markerIndex2(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex3.length) {
          entityRow = entityIndex3(i)
          markerIndex3(entityRow) = !markerIndex3(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex4.length) {
          entityRow = entityIndex4(i)
          markerIndex4(entityRow) = !markerIndex4(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex5.length) {
          entityRow = entityIndex5(i)
          markerIndex5(entityRow) = !markerIndex5(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex6.length) {
          entityRow = entityIndex6(i)
          markerIndex6(entityRow) = !markerIndex6(entityRow)
          i += 1
        }
        i = 0
        while (i < entityIndex7.length) {
          entityRow = entityIndex7(i)
          markerIndex7(entityRow) = !markerIndex7(entityRow)
          i += 1
        }
      }

      case n =>
        throw new IllegalArgumentException(
          "Can only mark up to 7 entity id columns. Found " + n
        )
    }
  }
}
