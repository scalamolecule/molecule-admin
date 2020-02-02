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
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global


case class Toggle(
  tableBody: TableSection,
  tpe: String,
  currentlyOn: Boolean,
  colIndex: Int = 0,
  eid: Long = 0L,
) extends AppElements {
  type keepBooPickleImport_SetMany = PickleState

  var entityRow               = 0
  var cells  : HTMLCollection = null
  var cell   : Element        = null
  val eidStrs                 = mutable.Set.empty[String]
  val allEids: Set[Long]      =
    if (eid == 0L) {
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
      filteredEidArray.flatten.map { eDouble =>
        eidStrs += eDouble.toString
        eDouble.toLong
      }.toSet
    } else {
      eidStrs += eid.toString
      Set(eid)
    }

  val (curMarkerIndexes, newCls, iconIndex, eids, count, toggling, toggled) =
    if (currentlyOn) {
      tpe match {
        case "star" =>
          val (starred, count) =
            if (eid == 0L) {
              val starred: Set[Long] = allEids.intersect(curStars)
              curStars --= starred
              (allEids.intersect(curStars), starred.size)
            } else {
              curStars -= eid
              (Set(eid), 1)
            }
          (curStarIndexes, mark.starOff, 1, starred, count, "Unstarring", "Unstarred")

        case "flag" =>
          val (flagged, count) =
            if (eid == 0L) {
              val flagged: Set[Long] = allEids.intersect(curFlags)
              curFlags --= flagged
              (flagged, flagged.size)
            } else {
              curFlags -= eid
              (Set(eid), 1)
            }
          (curFlagIndexes, mark.flagOff, 2, flagged, count, "Unflagging", "Unflagged")

        case "check" =>
          val (checked, count) =
            if (eid == 0L) {
              val checked: Set[Long] = allEids.intersect(curChecks)
              curChecks --= checked
              (checked, checked.size)
            } else {
              curChecks -= eid
              (Set(eid), 1)
            }
          (curCheckIndexes, mark.checkOff, 3, checked, count, "Unchecking", "Unchecked")
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
          (curStarIndexes, mark.starOn, 1, notStarred, count, "Starring", "Starred")

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
          (curFlagIndexes, mark.flagOn, 2, notFlagged, count, "Flagging", "Flagged")

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
          (curCheckIndexes, mark.checkOn, 3, notChecked, count, "Checking", "Checked")
      }
    }

  // Log
  if (count < 10000)
    print(s"$toggling $count entities in database...")
  else if (count < 100000)
    print(s"$toggling $count entities in database - can take a few seconds...")
  else
    print(s"$toggling $count entities in database - can take more than 5 seconds...")


  def toggleIcon(eidCol: Int): Unit = {
    cell = cells(eidCol)
    if (eidStrs.contains(cell.innerText))
      cell.children(iconIndex).setAttribute("class", newCls)
  }

  def save(): Unit = {

    // Update markers for each entity id column
    eidCols.foreach { eidCol =>

      // Loop affected entity ids
      eids.foreach { eid =>

        // Eid might not be present in (other) column
        val entityIndexOpt: Option[List[Int]] = curEntityIndexes(eidCol).get(eid)

        // Update column only if it contains eid
        entityIndexOpt.foreach { entityIndex =>
          val curMarkerIndex: Array[Boolean] = curMarkerIndexes(eidCol)
          var i                              = 0
          val entityIndexLength              = entityIndex.length
          while (i < entityIndexLength) {
            entityRow = entityIndex(i)
            curMarkerIndex(entityRow) = currentlyOn
            i += 1
          }
        }
      }
    }

    // Save asynchronously in meta db
    queryWire().saveToggle(db, dbSettingsIdOpt, tpe, eids, currentlyOn).call()
      .foreach {
        case Left(err)            => window.alert(err)
        case Right(dbSettingsId1) =>
          dbSettingsIdOpt = Some(dbSettingsId1)
          //          println(s"$toggled $count entity ids in database")
          println(" done")

      }
  }

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
}
