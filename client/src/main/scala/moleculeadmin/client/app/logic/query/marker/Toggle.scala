package moleculeadmin.client.app.logic.query.marker

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.util.HelpersAdmin
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.{Element, HTMLCollection}
import org.scalajs.dom.window
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global


case class Toggle(
  tableBody: TableSection,
  tpe: String,
  currentlyOn: Boolean,
  colIndex: Int = 0,
  eid: Long = 0L,
) extends AppElements with HelpersAdmin {
  type keepBooPickleImport_SetMany = PickleState

  var entityRow             = 0
  var cells: HTMLCollection = null
  var cell : Element        = null
  val allEids               = mutable.Set.empty[Long]

  val eidStrs = if (eid == 0L) {
    val qr               = queryCache.queryResult
    val eidArray         = qr.num(qr.arrayIndexes(colIndex))
    val filteredEidArray =
      if (cachedFilterIndex.nonEmpty) {
        val a2     = new Array[Option[Double]](cachedFilterIndex.length)
        var i      = 0
        val length = cachedFilterIndex.length
        while (i < length) {
          a2(i) = eidArray(cachedFilterIndex(i))
          i += 1
        }
        a2
      } else {
        eidArray
      }

    val eidStrs = new Array[String](filteredEidArray.length)
    var i       = 0
    filteredEidArray.foreach {
      case Some(eDouble) =>
        allEids += eDouble.toLong
        eidStrs(i) = eDouble.toString
        i += 1
      case _             =>
    }
    eidStrs
  } else {
    allEids += eid
    Array(eid.toString)
  }

  val (newCls, iconIndex, eids, count, toggling) = {
    if (currentlyOn) {
      tpe match {
        case "star" =>
          val (starred, count) =
            if (eid == 0L) {
              val starred = allEids.intersect(curStars)
              curStars --= starred
              (starred, starred.size)
            } else {
              curStars -= eid
              (Set(eid), 1)
            }
          (mark.starOff, 1, starred, count, "Unstarring")

        case "flag" =>
          val (flagged, count) =
            if (eid == 0L) {
              val flagged = allEids.intersect(curFlags)
              curFlags --= flagged
              (flagged, flagged.size)
            } else {
              curFlags -= eid
              (Set(eid), 1)
            }
          (mark.flagOff, 2, flagged, count, "Unflagging")

        case "check" =>
          val (checked, count) =
            if (eid == 0L) {
              val checked = allEids.intersect(curChecks)
              curChecks --= checked
              (checked, checked.size)
            } else {
              curChecks -= eid
              (Set(eid), 1)
            }
          (mark.checkOff, 3, checked, count, "Unchecking")
      }
    } else {
      tpe match {
        case "star" =>
          val (notStarred, count) =
            if (eid == 0L) {
              val notStarred = allEids.diff(curStars)
              curStars ++= notStarred
              (notStarred, notStarred.size)
            } else {
              curStars += eid
              (Set(eid), 1)
            }
          (mark.starOn, 1, notStarred, count, "Starring")

        case "flag" =>
          val (notFlagged, count) =
            if (eid == 0L) {
              val notFlagged = allEids.diff(curFlags)
              curFlags ++= notFlagged
              (notFlagged, notFlagged.size)
            } else {
              curFlags += eid
              (Set(eid), 1)
            }
          (mark.flagOn, 2, notFlagged, count, "Flagging")

        case "check" =>
          val (notChecked, count) =
            if (eid == 0L) {
              val notChecked = allEids.diff(curChecks)
              curChecks ++= notChecked
              (notChecked, notChecked.size)
            } else {
              curChecks += eid
              (Set(eid), 1)
            }
          (mark.checkOn, 3, notChecked, count, "Checking")
      }
    }
  }

  if (count > 1)
    print(s"$toggling $count entities in database")

  // Save in metaDb
  def save(): Unit = {
    queryWireAjax().saveToggle(db, dbSettingsIdOpt, tpe, eids, currentlyOn).call()
      .foreach {
        case Left(err)            => window.alert(err)
        case Right(dbSettingsId1) =>
          dbSettingsIdOpt = Some(dbSettingsId1)
      }
  }

  // Toggle of table row markers
  val rows = tableBody.children
  eidCols.length match {
    case 1 => {
      val c1         = eidCols.head
      var i          = 0
      val rowsLength = rows.length
      while (i < rowsLength) {
        cells = rows(i).children
        toggleIcon(c1)
        i += 1
      }
      save()
    }

    case 2 => {
      val Seq(c1, c2) = eidCols
      var i           = 0
      val rowsLength  = rows.length
      while (i < rowsLength) {
        cells = rows(i).children
        toggleIcon(c1)
        toggleIcon(c2)
        i += 1
      }
      save()
    }

    case 3 => {
      val Seq(c1, c2, c3) = eidCols
      var i               = 0
      val rowsLength      = rows.length
      while (i < rowsLength) {
        cells = rows(i).children
        toggleIcon(c1)
        toggleIcon(c2)
        toggleIcon(c3)
        i += 1
      }
      save()
    }

    case 4 => {
      val Seq(c1, c2, c3, c4) = eidCols
      var i                   = 0
      val rowsLength          = rows.length
      while (i < rowsLength) {
        cells = rows(i).children
        toggleIcon(c1)
        toggleIcon(c2)
        toggleIcon(c3)
        toggleIcon(c4)
        i += 1
      }
      save()
    }

    case 5 => {
      val Seq(c1, c2, c3, c4, c5) = eidCols
      var i                       = 0
      val rowsLength              = rows.length
      while (i < rowsLength) {
        cells = rows(i).children
        toggleIcon(c1)
        toggleIcon(c2)
        toggleIcon(c3)
        toggleIcon(c4)
        toggleIcon(c5)
        i += 1
      }
      save()
    }

    case 6 => {
      val Seq(c1, c2, c3, c4, c5, c6) = eidCols
      var i                           = 0
      val rowsLength                  = rows.length
      while (i < rowsLength) {
        cells = rows(i).children
        toggleIcon(c1)
        toggleIcon(c2)
        toggleIcon(c3)
        toggleIcon(c4)
        toggleIcon(c5)
        toggleIcon(c6)
        i += 1
      }
      save()
    }

    case 7 => {
      val Seq(c1, c2, c3, c4, c5, c6, c7) = eidCols
      var i                               = 0
      val rowsLength                      = rows.length
      while (i < rowsLength) {
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
      save()
    }

    case n =>
      throw new IllegalArgumentException(
        "Can only mark up to 7 entity id columns. Found " + n
      )
  }

  def toggleIcon(eidCol: Int): Unit = {
    cell = cells(eidCol)
    if (eidStrs.contains(cell.innerText))
      cell.children(iconIndex).setAttribute("class", newCls)
  }
}
