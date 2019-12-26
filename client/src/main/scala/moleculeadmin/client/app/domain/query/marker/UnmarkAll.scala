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

  var i = 0

  def apply(
    tableBody: TableSection,
    colIndex: Int,
    tpe: String
  )(implicit ctx: Ctx.Owner): Unit = {

    val (curMarkerIndexes, offCls, iconIndex, marked) = tpe match {
      case "star"  => (curStarIndexes, mark.starOff, 0, "Unstarred")
      case "flag"  => (curFlagIndexes, mark.flagOff, 1, "Unflagged")
      case "check" => (curCheckIndexes, mark.checkOff, 2, "Unchecked")
    }

    // Toggle off marker icon in all entity columns
    val rows : HTMLCollection = tableBody.children
    var cells: HTMLCollection = null
    while (i < rows.length) {
      cells = rows(i).children
      eidCols.foreach { col =>
        cells(col).children(iconIndex).setAttribute("class", offCls)
      }
      i += 1
    }

    val rowCount = queryCache.queryResult.rowCount

    // Save asynchronously in meta db
    queryWire().unmarkAll(db, dbSettingsIdOpt, tpe).call()
      .foreach {
        case Left(err)            => window.alert(err)
        case Right(dbSettingsId1) =>
          dbSettingsIdOpt = Some(dbSettingsId1)
          println(marked + s" $rowCount entity ids")
      }

    // Forget eids
    tpe match {
      case "star"  => curStars = Set.empty[Long]
      case "flag"  => curFlags = Set.empty[Long]
      case "check" => curChecks = Set.empty[Long]
    }

    // Set marker to false for all entity id columns
    eidCols.foreach { col =>
      // Update cached marker index
      val curMarkerIndex = curMarkerIndexes(col)
      i = 0
      while (i < curMarkerIndex.length) {
        curMarkerIndex(i) = false
        i += 1
      }
    }
  }
}
