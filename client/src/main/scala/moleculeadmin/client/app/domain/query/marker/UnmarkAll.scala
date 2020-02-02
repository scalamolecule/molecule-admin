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


object UnmarkAll extends AppElements {
  type keepBooPickleImport_UnmarkAll = PickleState

  def apply(
    tableBody: TableSection,
    colIndex: Int,
    tpe: String
  )(implicit ctx: Ctx.Owner): Unit = {

    val (curMarkerIndexes, offCls, iconIndex, marked, count) = tpe match {
      case "star"  => (curStarIndexes, mark.starOff, 1, "Unstarred", curStars.size)
      case "flag"  => (curFlagIndexes, mark.flagOff, 2, "Unflagged", curFlags.size)
      case "check" => (curCheckIndexes, mark.checkOff, 3, "Unchecked", curChecks.size)
    }

    // Toggle off marker icon in all entity columns
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

    // Set marker to false for all entity id columns
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
    queryWire().unmarkAll(db, dbSettingsIdOpt, tpe).call()
      .foreach {
        case Right(dbSettingsId1) =>
          dbSettingsIdOpt = Some(dbSettingsId1)
          println(s"$marked $count entity ids")
        case Left(err)            => window.alert(err)
      }

    // Forget eids
    tpe match {
      case "star"  => curStars = Set.empty[Long]
      case "flag"  => curFlags = Set.empty[Long]
      case "check" => curChecks = Set.empty[Long]
    }
  }
}
