package moleculeadmin.client.app.domain.query.marker


import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.html.TableSection
import org.scalajs.dom.raw.{Element, HTMLCollection}
import rx.Ctx
import org.scalajs.dom.{document, window}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global


object UnmarkAll extends AppElements {
  type keepBooPickleImport = PickleState

  var cells: HTMLCollection = null
  var cell : Element        = null
  var i                     = 0
  var entityRow             = 0
  var eid                   = 0L
  var eids                  = Set.empty[Long]
  lazy val eidStrs = eids.map(_.toString)


  def apply(
    db: String,
    tableBody: TableSection,
    colIndex: Int,
    tpe: String
  )(implicit ctx: Ctx.Owner): Unit = {

    val (curMarkerIndexes, offCls, iconIndex) = tpe match {
      case "star"  => (curStarIndexes, mark.starOff, 0)
      case "flag"  => (curFlagIndexes, mark.flagOff, 1)
      case "check" => (curCheckIndexes, mark.checkOff, 2)
    }

    val tableCol    = colIndex + 1
    val rows        = tableBody.children
    val qr          = queryCache.now.find(_.modelElements == modelElements.now).get.queryResult
    val eidArray    = qr.num(qr.arrayIndexes(colIndex))
    val arrayLength = eidArray.length


    def save(): Unit = {
      queryWire().unmarkAll(db, dbSettingsIdOpt, tpe).call().foreach {
        case Left(err)            => window.alert(err)
        case Right(dbSettingsId1) =>
          dbSettingsIdOpt = Some(dbSettingsId1)
          println(s"Toggled off all ${eids.size} entities")
      }
    }

    def setMarkerClass(): Unit = {
      cells(tableCol).children(iconIndex).setAttribute("class", offCls)
    }

    def setOtherMarkerClass(otherTableCol: Int): Unit = {
      cell = cells(otherTableCol)
      if (eidStrs.contains(cell.innerText))
        cell.children(iconIndex).setAttribute("class", offCls)
    }

    def collectEidsLambda(): Int => Unit =
      (i: Int) => eidArray(i).foreach(eids += _.toLong)

    val resetMarker = tpe match {
      case "star"  => () => curStars = Set.empty[Long]
      case "flag"  => () => curFlags = Set.empty[Long]
      case "check" => () => curChecks = Set.empty[Long]
    }

    def updateOtherMarkerIndex(tableCol: Int): Unit = {
      // Loop affected entity ids
      eids.foreach { eid =>
        // Eid might not be present in other column
        val entityIndexOpt = curEntityIndexes(tableCol).get(eid)
        val markerIndex    = curMarkerIndexes(tableCol)
        entityIndexOpt.foreach { entityIndex =>
          i = 0
          while (i < entityIndex.length) {
            entityRow = entityIndex(i)
            markerIndex(entityRow) = !markerIndex(entityRow)
            i += 1
          }

          // remove cached entity indexes
          curEntityIndexes(tableCol) = curEntityIndexes(tableCol).-=(eid)
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

        resetMarker()

        val collectEids = collectEidsLambda()
        save()

        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < curMarkerIndex.length) {
          // Collect eids
          collectEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = false
          i += 1
        }
      }

      case 2 => {
        val collectEids = collectEidsLambda()
        save()
        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < arrayLength) {
          // Collect eids
          collectEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = false
          i += 1
        }

        resetMarker()

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
        val collectEids = collectEidsLambda()
        save()
        val curMarkerIndex = curMarkerIndexes(tableCol)
        i = 0
        while (i < arrayLength) {
          // Collect eids
          collectEids(i)
          // Update curMarkerIndex
          curMarkerIndex(i) = false
          i += 1
        }

        resetMarker()

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


      case n =>
        throw new IllegalArgumentException(
          "Can only mark up to 7 entity id columns. Found " + n
        )
    }
  }
}
