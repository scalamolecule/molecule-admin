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
import org.scalajs.dom.{document, window}
import rx.Ctx
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global


case class ToggleMany(
  db: String,
  tableBody: TableSection,
  colIndex: Int,
  tpe: String,
)(implicit val ctx: Ctx.Owner) extends AppElements {

  type keepBooPickleImport = PickleState

  val (curMarkerIndexes, onCls, offCls, iconIndex) = tpe match {
    case "star"  => (curStarIndexes, mark.starOn, mark.starOff, 0)
    case "flag"  => (curFlagIndexes, mark.flagOn, mark.flagOff, 1)
    case "check" => (curCheckIndexes, mark.checkOn, mark.checkOff, 2)
  }

  // Placeholders to minimize new object memory allocations
  var cells: HTMLCollection = null
  var cell : Element        = null
  var i                     = 0
  var entityRow             = 0
  var eid                   = 0L
  var eids                  = Set.empty[Long]
  lazy val eidStrs = eids.map(_.toString)

  val tableCol    = colIndex + 1
  val rows        = tableBody.children
  val qr          = queryCache.now.find(_.modelElements == modelElements.now).get.queryResult
  val eidArray    = qr.num(qr.arrayIndexes(colIndex))
  val arrayLength = eidArray.length


  def set(newState: Boolean): Unit = {
    val newCls = if (newState) onCls else offCls

    def save(): Unit = {
      queryWire().toggleMarkers(db, dbSettingsIdOpt, tpe, eids, newState).call().foreach {
        case Left(err)            => window.alert(err)
        case Right(dbSettingsId1) =>
          dbSettingsIdOpt = Some(dbSettingsId1)
          println(s"Toggled ${eids.size} entities to " + newState)
      }
    }

    def setMarkerClass(): Unit = {
      cells(tableCol).children(iconIndex).setAttribute("class", newCls)
    }

    def setOtherMarkerClass(otherTableCol: Int): Unit = {
      cell = cells(otherTableCol)
      if (eidStrs.contains(cell.innerText))
        cell.children(iconIndex).setAttribute("class", newCls)
    }

    def toggleCurEidsLambda(): Int => Unit = {
      if (newState)
        tpe match {
          case "star"  => (i: Int) =>
            eidArray(i).foreach { double =>
              eid = double.toLong
              curStars += eid
              eids += eid
            }
          case "flag"  => (i: Int) =>
            eidArray(i).foreach { double =>
              eid = double.toLong
              curFlags += eid
              eids += eid
            }
          case "check" => (i: Int) =>
            eidArray(i).foreach { double =>
              eid = double.toLong
              curChecks += eid
              eids += eid
            }
        }
      else
        tpe match {
          case "star"  => (i: Int) =>
            eidArray(i).foreach { double =>
              eid = double.toLong
              curStars -= eid
              eids += eid
            }
          case "flag"  => (i: Int) =>
            eidArray(i).foreach { double =>
              eid = double.toLong
              curFlags -= eid
              eids += eid
            }
          case "check" => (i: Int) =>
            eidArray(i).foreach { double =>
              eid = double.toLong
              curChecks -= eid
              eids += eid
            }
        }
    }

    def updateOtherMarkerIndex(tableCol: Int): Unit = {
      // Loop affected entity ids
      eids.foreach { eid =>
        // Eid might not be present in other column
        val entityIndexOpt: Option[List[Int]] = curEntityIndexes(tableCol).get(eid)
        val markerIndex                       = curMarkerIndexes(tableCol)
        entityIndexOpt.foreach { entityIndex =>
          i = 0
          while (i < entityIndex.length) {
            entityRow = entityIndex(i)
            markerIndex(entityRow) = !markerIndex(entityRow)
            i += 1
          }
          if (!newState) {
            // remove cached entity indexes
            curEntityIndexes(tableCol) = curEntityIndexes(tableCol).-=(eid)
          }
        }
      }
    }


    eTableColIndexes.length match {
      case 1 => {
        // With only 1 eid column we can toggle all visible first
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass()
          i += 1
        }

        val toggleCurEids  = toggleCurEidsLambda()
        save()

        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < curMarkerIndex.length) {
          // Update curMarkers and collect eids
          toggleCurEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = newState
          i += 1
        }
      }

      case 2 => {
        val toggleCurEids  = toggleCurEidsLambda()
        save()
        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < arrayLength) {
          // Update curMarkers and collect eids
          toggleCurEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = newState
          i += 1
        }

        // Other entity id columns
        val c2 = eTableColIndexes.filterNot(_ == tableCol).head

        // Update visible rows
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass()
          setOtherMarkerClass(c2)
          i += 1
        }

        // Update other markerIndex
        updateOtherMarkerIndex(c2)
      }

      case 3 => {
        val toggleCurEids  = toggleCurEidsLambda()
        save()
        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < arrayLength) {
          // Update curMarkers and collect eids
          toggleCurEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = newState
          i += 1
        }

        // Other entity id columns
        val Seq(c2, c3) = eTableColIndexes.filterNot(_ == tableCol)

        // Update visible rows
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass()
          setOtherMarkerClass(c2)
          setOtherMarkerClass(c3)
          i += 1
        }

        // Update other markerIndex
        updateOtherMarkerIndex(c2)
        updateOtherMarkerIndex(c3)
      }

      case 4 => {
        val toggleCurEids  = toggleCurEidsLambda()
        save()
        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < arrayLength) {
          // Update curMarkers and collect eids
          toggleCurEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = newState
          i += 1
        }

        // Other entity id columns
        val Seq(c2, c3, c4) = eTableColIndexes.filterNot(_ == tableCol)

        // Update visible rows
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass()
          setOtherMarkerClass(c2)
          setOtherMarkerClass(c3)
          setOtherMarkerClass(c4)
          i += 1
        }

        // Update other markerIndex
        updateOtherMarkerIndex(c2)
        updateOtherMarkerIndex(c3)
        updateOtherMarkerIndex(c4)
      }

      case 5 => {
        val toggleCurEids  = toggleCurEidsLambda()
        save()
        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < arrayLength) {
          // Update curMarkers and collect eids
          toggleCurEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = newState
          i += 1
        }

        // Other entity id columns
        val Seq(c2, c3, c4, c5) = eTableColIndexes.filterNot(_ == tableCol)

        // Update visible rows
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass()
          setOtherMarkerClass(c2)
          setOtherMarkerClass(c3)
          setOtherMarkerClass(c4)
          setOtherMarkerClass(c5)
          i += 1
        }

        // Update other markerIndex
        updateOtherMarkerIndex(c2)
        updateOtherMarkerIndex(c3)
        updateOtherMarkerIndex(c4)
        updateOtherMarkerIndex(c5)
      }

      case 6 => {
        val toggleCurEids  = toggleCurEidsLambda()
        save()
        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < arrayLength) {
          // Update curMarkers and collect eids
          toggleCurEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = newState
          i += 1
        }

        // Other entity id columns
        val Seq(c2, c3, c4, c5, c6) = eTableColIndexes.filterNot(_ == tableCol)

        // Update visible rows
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass()
          setOtherMarkerClass(c2)
          setOtherMarkerClass(c3)
          setOtherMarkerClass(c4)
          setOtherMarkerClass(c5)
          setOtherMarkerClass(c6)
          i += 1
        }

        // Update other markerIndex
        updateOtherMarkerIndex(c2)
        updateOtherMarkerIndex(c3)
        updateOtherMarkerIndex(c4)
        updateOtherMarkerIndex(c5)
        updateOtherMarkerIndex(c6)
      }

      case 7 => {
        val toggleCurEids  = toggleCurEidsLambda()
        save()
        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < arrayLength) {
          // Update curMarkers and collect eids
          toggleCurEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = newState
          i += 1
        }

        // Other entity id columns
        val Seq(c2, c3, c4, c5, c6, c7) = eTableColIndexes.filterNot(_ == tableCol)

        // Update visible rows
        i = 0
        while (i < rows.length) {
          cells = rows(i).children
          setMarkerClass()
          setOtherMarkerClass(c2)
          setOtherMarkerClass(c3)
          setOtherMarkerClass(c4)
          setOtherMarkerClass(c5)
          setOtherMarkerClass(c6)
          setOtherMarkerClass(c7)
          i += 1
        }

        // Update other markerIndex
        updateOtherMarkerIndex(c2)
        updateOtherMarkerIndex(c3)
        updateOtherMarkerIndex(c4)
        updateOtherMarkerIndex(c5)
        updateOtherMarkerIndex(c6)
        updateOtherMarkerIndex(c7)
      }

      case n =>
        throw new IllegalArgumentException(
          "Can only mark up to 7 entity id columns. Found " + n
        )
    }
  }
}
