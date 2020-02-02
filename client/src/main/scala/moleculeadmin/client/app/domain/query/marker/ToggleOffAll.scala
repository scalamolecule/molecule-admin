package moleculeadmin.client.app.domain.query.marker

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.HTMLCollection
import org.scalajs.dom.window
import rx.Ctx
import scala.concurrent.ExecutionContext.Implicits.global


case class ToggleOffAll(tableBody: TableSection, tpe: String) extends AppElements {
  type keepBooPickleImport_UnmarkAll = PickleState


  val (curMarkerIndexes, offCls, iconIndex, eids, count, toggling, toggled) =
    tpe match {
      case "star" =>
        val starred = curStars
        curStars = Set.empty[Long]
        (curStarIndexes, mark.starOff, 1, starred, starred.size,
          "Unstarring all", "Unstarred all")

      case "flag" =>
        val flagged = curFlags
        curFlags = Set.empty[Long]
        (curFlagIndexes, mark.flagOff, 2, flagged, flagged.size,
          "Unflagging all", "Unflagged all")

      case "check" =>
        val checked = curChecks
        curChecks = Set.empty[Long]
        (curCheckIndexes, mark.checkOff, 3, checked, checked.size,
          "Unchecking all", "Unchecked all")
    }

  // Log
  if (count < 10000)
    print(s"$toggling $count entities in database ...")
  else if (count < 100000)
    print(s"$toggling $count entities in database - can take a few seconds ...")
  else
    print(s"$toggling $count entities in database - can take more than 5 seconds ...")


  // Toggle off specific marker icon in all entity columns
  val rows : HTMLCollection = tableBody.children
  var cells: HTMLCollection = null
  var i                     = 0
  while (i < rows.length) {
    cells = rows(i).children
    eidCols.foreach { col =>
      cells(col).children(iconIndex).setAttribute("class", offCls)
    }
    i += 1
  }

  // Set current marker index to false for all entity id columns
  eidCols.foreach { col =>
    // Update cached marker index
    val curMarkerIndex       = curMarkerIndexes(col)
    var i                    = 0
    val curMarkerIndexLength = curMarkerIndex.length
    while (i < curMarkerIndexLength) {
      curMarkerIndex(i) = false
      i += 1
    }
  }

  // Save asynchronously in meta db
  queryWire().saveToggle(db, dbSettingsIdOpt, tpe, eids, false).call()
    .foreach {
      case Right(dbSettingsId1) =>
        dbSettingsIdOpt = Some(dbSettingsId1)
        println(" done")
      case Left(err)            => window.alert(err)
    }
}
