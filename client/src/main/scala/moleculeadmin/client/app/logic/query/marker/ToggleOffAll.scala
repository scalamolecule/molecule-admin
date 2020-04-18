package moleculeadmin.client.app.logic.query.marker

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.queryWireAjax
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.HTMLCollection
import org.scalajs.dom.window
import rx.Ctx
import scala.concurrent.ExecutionContext.Implicits.global


case class ToggleOffAll(tableBody: TableSection, tpe: String) extends AppElements {

  val (offCls, iconIndex, eids, count, toggling) =
    tpe match {
      case "star" =>
        val starred = curStars
        curStars = Set.empty[Long]
        (mark.starOff, 1, starred, starred.size, "Unstarring all")

      case "flag" =>
        val flagged = curFlags
        curFlags = Set.empty[Long]
        (mark.flagOff, 2, flagged, flagged.size, "Unflagging all")

      case "check" =>
        val checked = curChecks
        curChecks = Set.empty[Long]
        (mark.checkOff, 3, checked, checked.size, "Unchecking all")
    }

  if (count > 1)
    println(s"$toggling $count entities in database")

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

  // Save asynchronously in meta db
  queryWireAjax().saveToggle(db, dbSettingsIdOpt, tpe, eids, true).call()
    .foreach {
      case Right(dbSettingsId1) =>
        dbSettingsIdOpt = Some(dbSettingsId1)
      case Left(err)            => window.alert(err)
    }
}
