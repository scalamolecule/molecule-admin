package moleculeadmin.client.app.domain.query.marker

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState.{cachedFilterIndex, curFlags, _}
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.{Element, HTMLCollection}
import org.scalajs.dom.window
import rx.Ctx
import scala.concurrent.ExecutionContext.Implicits.global


object SetMany extends AppElements {
  type keepBooPickleImport_SetMany = PickleState

  var entityRow             = 0
  var cells: HTMLCollection = null
  var cell : Element        = null

  def apply(
    tableBody: TableSection,
    colIndex: Int,
    tpe: String,
    newState: Boolean
  )(implicit ctx: Ctx.Owner): Unit = {

    val allEids: Set[Long] = {
      val qr = queryCache.queryResult

      val eidArray: Array[Option[Double]] = qr.num(qr.arrayIndexes(colIndex))

      val filteredEidArray: Array[Option[Double]] =
        if (cachedFilterIndex.nonEmpty) {
          val a2                      = new Array[Option[Double]](cachedFilterIndex.length)
          var i                       = 0
          val cachedFilterIndexLength = cachedFilterIndex.length
          while (i < cachedFilterIndexLength) {
            a2(i) = eidArray(cachedFilterIndex(i))
            i += 1
          }
          a2
        } else {
          eidArray
        }

      filteredEidArray.flatten.map(_.toLong).toSet
    }

    val (curMarkerIndexes, newCls, iconIndex, eids, marked) =
      if (newState) {
        tpe match {
          case "star"  =>
            val notStarred = allEids.diff(curStars)
            curStars ++= notStarred
            (curStarIndexes, mark.starOn, 1, notStarred, "Starred " + notStarred.size)

          case "flag"  =>
            val notFlagged = allEids.diff(curFlags)
            curFlags ++= notFlagged
            (curFlagIndexes, mark.flagOn, 2, notFlagged, "Flagged " + notFlagged.size)

          case "check" =>
            val notChecked = allEids.diff(curChecks)
            curChecks ++= notChecked
            (curCheckIndexes, mark.checkOn, 3, notChecked, "Checked " + notChecked.size)
        }
      } else {
        tpe match {
          case "star"  =>
            val starred = allEids.intersect(curStars)
            curStars --= starred
            (curStarIndexes, mark.starOff, 1, starred, "Unstarred " + starred.size)

          case "flag"  =>
            val flagged = allEids.intersect(curFlags)
            curFlags --= flagged
            (curFlagIndexes, mark.flagOff, 2, flagged, "Unflagged " + flagged.size)

          case "check" =>
            val checked = allEids.intersect(curChecks)
            curChecks --= checked
            (curCheckIndexes, mark.checkOff, 3, checked, "Unchecked " + checked.size)
        }
      }

    val tableCol = colIndex + 1

    def toggleIconCurCol(): Unit = {
      cells(tableCol).children(iconIndex).setAttribute("class", newCls)
    }

    def toggleIconOtherCol(otherTableCol: Int): Unit = {
      val eidStrs = allEids.map(_.toString)
      cell = cells(otherTableCol)
      // Toggle matching entity ids
      if (eidStrs.contains(cell.innerText))
        cell.children(iconIndex).setAttribute("class", newCls)
    }

    def save(): Unit = {

      // Update markers for each entity id column
      eidCols.foreach { col =>

        // Loop affected entity ids
        eids.foreach { eid =>

          // Eid might not be present in (other) column
          val entityIndexOpt: Option[List[Int]] =
            curEntityIndexes(col).get(eid)

          // Update column only if it contains eid
          entityIndexOpt.foreach { entityIndex =>
            val curMarkerIndex: Array[Boolean] = curMarkerIndexes(col)
            var i                              = 0
            val entityIndexLength              = entityIndex.length
            while (i < entityIndexLength) {
              entityRow = entityIndex(i)
              curMarkerIndex(entityRow) = newState
              i += 1
            }
          }
        }
      }

      // Save asynchronously in meta db
      queryWire().setMarkers(db, dbSettingsIdOpt, tpe, eids, newState).call()
        .foreach {
          case Left(err)            => window.alert(err)
          case Right(dbSettingsId1) =>
            dbSettingsIdOpt = Some(dbSettingsId1)
            println(marked + " entity ids")
        }
    }

    val rows = tableBody.children

    eidCols.length match {
      case 1 => {
        // With only 1 eid column we can toggle table rows first
        var i          = 0
        val rowsLength = rows.length
        while (i < rowsLength) {
          cells = rows(i).children
          toggleIconCurCol()
          i += 1
        }
        save()
      }

      case 2 => {
        // Other entity id columns
        val c2 = eidCols.filterNot(_ == tableCol).head

        // Update table rows
        var i          = 0
        val rowsLength = rows.length
        while (i < rowsLength) {
          cells = rows(i).children
          toggleIconCurCol()
          toggleIconOtherCol(c2)
          i += 1
        }
        save()
      }

      case 3 => {
        val Seq(c2, c3) = eidCols.filterNot(_ == tableCol)
        var i           = 0
        val rowsLength  = rows.length
        while (i < rowsLength) {
          cells = rows(i).children
          toggleIconCurCol()
          toggleIconOtherCol(c2)
          toggleIconOtherCol(c3)
          i += 1
        }
        save()
      }

      case 4 => {
        val Seq(c2, c3, c4) = eidCols.filterNot(_ == tableCol)
        var i               = 0
        val rowsLength      = rows.length
        while (i < rowsLength) {
          cells = rows(i).children
          toggleIconCurCol()
          toggleIconOtherCol(c2)
          toggleIconOtherCol(c3)
          toggleIconOtherCol(c4)
          i += 1
        }
        save()
      }

      case 5 => {
        val Seq(c2, c3, c4, c5) = eidCols.filterNot(_ == tableCol)
        var i                   = 0
        val rowsLength          = rows.length
        while (i < rowsLength) {
          cells = rows(i).children
          toggleIconCurCol()
          toggleIconOtherCol(c2)
          toggleIconOtherCol(c3)
          toggleIconOtherCol(c4)
          toggleIconOtherCol(c5)
          i += 1
        }
        save()
      }

      case 6 => {
        val Seq(c2, c3, c4, c5, c6) = eidCols.filterNot(_ == tableCol)
        var i                       = 0
        val rowsLength              = rows.length
        while (i < rowsLength) {
          cells = rows(i).children
          toggleIconCurCol()
          toggleIconOtherCol(c2)
          toggleIconOtherCol(c3)
          toggleIconOtherCol(c4)
          toggleIconOtherCol(c5)
          toggleIconOtherCol(c6)
          i += 1
        }
        save()
      }

      case 7 => {
        val Seq(c2, c3, c4, c5, c6, c7) = eidCols.filterNot(_ == tableCol)
        var i                           = 0
        val rowsLength                  = rows.length
        while (i < rowsLength) {
          cells = rows(i).children
          toggleIconCurCol()
          toggleIconOtherCol(c2)
          toggleIconOtherCol(c3)
          toggleIconOtherCol(c4)
          toggleIconOtherCol(c5)
          toggleIconOtherCol(c6)
          toggleIconOtherCol(c7)
          i += 1
        }
        save()
      }

      case n =>
        throw new IllegalArgumentException(
          "Can only mark up to 7 entity id columns. Found " + n
        )
    }
  }

}
