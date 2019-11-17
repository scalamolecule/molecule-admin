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


case class Marker(
  db: String,
  tableBody: TableSection,
  eColIndexes: Seq[Int],
  tpe: String
) extends AppElements {

  type keepBooPickleImport = PickleState

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
      curMarker = curMarker + eid
      onCls
    }

    queryWire().toggleMarker(db, dbSettingsIdOpt, tpe, eid, isOn).call().foreach {
      case Left(err)            => window.alert(err)
      case Right(dbSettingsId1) =>
        dbSettingsIdOpt = Some(dbSettingsId1)
        println(s"Eid $eid $tpe: " + !isOn)
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

        val entityIndex1opt = curEntityIndexes(c1).get(eid)
        val markerIndex1    = curMarkerIndexes(c1)

        entityIndex1opt.foreach { entityIndex1 =>
          i = 0
          while (i < entityIndex1.length) {
            entityRow = entityIndex1(i)
            markerIndex1(entityRow) = !markerIndex1(entityRow)
            i += 1
          }
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

        val entityIndex1opt = curEntityIndexes(c1).get(eid)
        val entityIndex2opt = curEntityIndexes(c2).get(eid)
        val markerIndex1    = curMarkerIndexes(c1)
        val markerIndex2    = curMarkerIndexes(c2)

        entityIndex1opt.foreach { entityIndex1 =>
          i = 0
          while (i < entityIndex1.length) {
            entityRow = entityIndex1(i)
            markerIndex1(entityRow) = !markerIndex1(entityRow)
            i += 1
          }
        }
        entityIndex2opt.foreach { entityIndex2 =>
          i = 0
          while (i < entityIndex2.length) {
            entityRow = entityIndex2(i)
            markerIndex2(entityRow) = !markerIndex2(entityRow)
            i += 1
          }
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

        val entityIndex1opt = curEntityIndexes(c1).get(eid)
        val entityIndex2opt = curEntityIndexes(c2).get(eid)
        val entityIndex3opt = curEntityIndexes(c3).get(eid)
        val markerIndex1    = curMarkerIndexes(c1)
        val markerIndex2    = curMarkerIndexes(c2)
        val markerIndex3    = curMarkerIndexes(c3)

        entityIndex1opt.foreach { entityIndex1 =>
          i = 0
          while (i < entityIndex1.length) {
            entityRow = entityIndex1(i)
            markerIndex1(entityRow) = !markerIndex1(entityRow)
            i += 1
          }
        }
        entityIndex2opt.foreach { entityIndex2 =>
          i = 0
          while (i < entityIndex2.length) {
            entityRow = entityIndex2(i)
            markerIndex2(entityRow) = !markerIndex2(entityRow)
            i += 1
          }
        }
        entityIndex3opt.foreach { entityIndex3 =>
          i = 0
          while (i < entityIndex3.length) {
            entityRow = entityIndex3(i)
            markerIndex3(entityRow) = !markerIndex3(entityRow)
            i += 1
          }
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

        val entityIndex1opt = curEntityIndexes(c1).get(eid)
        val entityIndex2opt = curEntityIndexes(c2).get(eid)
        val entityIndex3opt = curEntityIndexes(c3).get(eid)
        val entityIndex4opt = curEntityIndexes(c4).get(eid)
        val markerIndex1    = curMarkerIndexes(c1)
        val markerIndex2    = curMarkerIndexes(c2)
        val markerIndex3    = curMarkerIndexes(c3)
        val markerIndex4    = curMarkerIndexes(c4)

        entityIndex1opt.foreach { entityIndex1 =>
          i = 0
          while (i < entityIndex1.length) {
            entityRow = entityIndex1(i)
            markerIndex1(entityRow) = !markerIndex1(entityRow)
            i += 1
          }
        }

        entityIndex2opt.foreach { entityIndex2 =>
          i = 0
          while (i < entityIndex2.length) {
            entityRow = entityIndex2(i)
            markerIndex2(entityRow) = !markerIndex2(entityRow)
            i += 1
          }
        }
        entityIndex3opt.foreach { entityIndex3 =>
          i = 0
          while (i < entityIndex3.length) {
            entityRow = entityIndex3(i)
            markerIndex3(entityRow) = !markerIndex3(entityRow)
            i += 1
          }
        }
        entityIndex4opt.foreach { entityIndex4 =>
          i = 0
          while (i < entityIndex4.length) {
            entityRow = entityIndex4(i)
            markerIndex4(entityRow) = !markerIndex4(entityRow)
            i += 1
          }
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

        val entityIndex1opt = curEntityIndexes(c1).get(eid)
        val entityIndex2opt = curEntityIndexes(c2).get(eid)
        val entityIndex3opt = curEntityIndexes(c3).get(eid)
        val entityIndex4opt = curEntityIndexes(c4).get(eid)
        val entityIndex5opt = curEntityIndexes(c5).get(eid)
        val markerIndex1    = curMarkerIndexes(c1)
        val markerIndex2    = curMarkerIndexes(c2)
        val markerIndex3    = curMarkerIndexes(c3)
        val markerIndex4    = curMarkerIndexes(c4)
        val markerIndex5    = curMarkerIndexes(c5)

        entityIndex1opt.foreach { entityIndex1 =>
          i = 0
          while (i < entityIndex1.length) {
            entityRow = entityIndex1(i)
            markerIndex1(entityRow) = !markerIndex1(entityRow)
            i += 1
          }
        }
        entityIndex2opt.foreach { entityIndex2 =>
          i = 0
          while (i < entityIndex2.length) {
            entityRow = entityIndex2(i)
            markerIndex2(entityRow) = !markerIndex2(entityRow)
            i += 1
          }
        }
        entityIndex3opt.foreach { entityIndex3 =>
          i = 0
          while (i < entityIndex3.length) {
            entityRow = entityIndex3(i)
            markerIndex3(entityRow) = !markerIndex3(entityRow)
            i += 1
          }
        }
        entityIndex4opt.foreach { entityIndex4 =>
          i = 0
          while (i < entityIndex4.length) {
            entityRow = entityIndex4(i)
            markerIndex4(entityRow) = !markerIndex4(entityRow)
            i += 1
          }
        }
        entityIndex5opt.foreach { entityIndex5 =>
          i = 0
          while (i < entityIndex5.length) {
            entityRow = entityIndex5(i)
            markerIndex5(entityRow) = !markerIndex5(entityRow)
            i += 1
          }
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

        val entityIndex1opt = curEntityIndexes(c1).get(eid)
        val entityIndex2opt = curEntityIndexes(c2).get(eid)
        val entityIndex3opt = curEntityIndexes(c3).get(eid)
        val entityIndex4opt = curEntityIndexes(c4).get(eid)
        val entityIndex5opt = curEntityIndexes(c5).get(eid)
        val entityIndex6opt = curEntityIndexes(c6).get(eid)
        val markerIndex1    = curMarkerIndexes(c1)
        val markerIndex2    = curMarkerIndexes(c2)
        val markerIndex3    = curMarkerIndexes(c3)
        val markerIndex4    = curMarkerIndexes(c4)
        val markerIndex5    = curMarkerIndexes(c5)
        val markerIndex6    = curMarkerIndexes(c6)

        entityIndex1opt.foreach { entityIndex1 =>
          i = 0
          while (i < entityIndex1.length) {
            entityRow = entityIndex1(i)
            markerIndex1(entityRow) = !markerIndex1(entityRow)
            i += 1
          }
        }
        entityIndex2opt.foreach { entityIndex2 =>
          i = 0
          while (i < entityIndex2.length) {
            entityRow = entityIndex2(i)
            markerIndex2(entityRow) = !markerIndex2(entityRow)
            i += 1
          }
        }
        entityIndex3opt.foreach { entityIndex3 =>
          i = 0
          while (i < entityIndex3.length) {
            entityRow = entityIndex3(i)
            markerIndex3(entityRow) = !markerIndex3(entityRow)
            i += 1
          }
        }
        entityIndex4opt.foreach { entityIndex4 =>
          i = 0
          while (i < entityIndex4.length) {
            entityRow = entityIndex4(i)
            markerIndex4(entityRow) = !markerIndex4(entityRow)
            i += 1
          }
        }
        entityIndex5opt.foreach { entityIndex5 =>
          i = 0
          while (i < entityIndex5.length) {
            entityRow = entityIndex5(i)
            markerIndex5(entityRow) = !markerIndex5(entityRow)
            i += 1
          }
        }
        entityIndex6opt.foreach { entityIndex6 =>
          i = 0
          while (i < entityIndex6.length) {
            entityRow = entityIndex6(i)
            markerIndex6(entityRow) = !markerIndex6(entityRow)
            i += 1
          }
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

        val entityIndex1opt = curEntityIndexes(c1).get(eid)
        val entityIndex2opt = curEntityIndexes(c2).get(eid)
        val entityIndex3opt = curEntityIndexes(c3).get(eid)
        val entityIndex4opt = curEntityIndexes(c4).get(eid)
        val entityIndex5opt = curEntityIndexes(c5).get(eid)
        val entityIndex6opt = curEntityIndexes(c6).get(eid)
        val entityIndex7opt = curEntityIndexes(c7).get(eid)
        val markerIndex1    = curMarkerIndexes(c1)
        val markerIndex2    = curMarkerIndexes(c2)
        val markerIndex3    = curMarkerIndexes(c3)
        val markerIndex4    = curMarkerIndexes(c4)
        val markerIndex5    = curMarkerIndexes(c5)
        val markerIndex6    = curMarkerIndexes(c6)
        val markerIndex7    = curMarkerIndexes(c7)

        entityIndex1opt.foreach { entityIndex1 =>
          i = 0
          while (i < entityIndex1.length) {
            entityRow = entityIndex1(i)
            markerIndex1(entityRow) = !markerIndex1(entityRow)
            i += 1
          }
        }
        entityIndex2opt.foreach { entityIndex2 =>
          i = 0
          while (i < entityIndex2.length) {
            entityRow = entityIndex2(i)
            markerIndex2(entityRow) = !markerIndex2(entityRow)
            i += 1
          }
        }
        entityIndex3opt.foreach { entityIndex3 =>
          i = 0
          while (i < entityIndex3.length) {
            entityRow = entityIndex3(i)
            markerIndex3(entityRow) = !markerIndex3(entityRow)
            i += 1
          }
        }
        entityIndex4opt.foreach { entityIndex4 =>
          i = 0
          while (i < entityIndex4.length) {
            entityRow = entityIndex4(i)
            markerIndex4(entityRow) = !markerIndex4(entityRow)
            i += 1
          }
        }
        entityIndex5opt.foreach { entityIndex5 =>
          i = 0
          while (i < entityIndex5.length) {
            entityRow = entityIndex5(i)
            markerIndex5(entityRow) = !markerIndex5(entityRow)
            i += 1
          }
        }
        entityIndex6opt.foreach { entityIndex6 =>
          i = 0
          while (i < entityIndex6.length) {
            entityRow = entityIndex6(i)
            markerIndex6(entityRow) = !markerIndex6(entityRow)
            i += 1
          }
        }
        entityIndex7opt.foreach { entityIndex7 =>
          i = 0
          while (i < entityIndex7.length) {
            entityRow = entityIndex7(i)
            markerIndex7(entityRow) = !markerIndex7(entityRow)
            i += 1
          }
        }
      }

      case n =>
        throw new IllegalArgumentException(
          "Can only mark up to 7 entity id columns. Found " + n
        )
    }
  }
}
