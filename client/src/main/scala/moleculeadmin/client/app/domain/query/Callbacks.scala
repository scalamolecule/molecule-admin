package moleculeadmin.client.app.domain.query
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.QueryDTO
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.ops.transform.Molecule2Model
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{document, window}
import rx.{Ctx, Rx}
import scala.concurrent.ExecutionContext.Implicits.global


class Callbacks(implicit ctx: Ctx.Owner)
  extends RxBindings with ColOps {

  type keepBooPickleImport_Callbacks = PickleState

  // Prevent multiple asynchronous calls to db while processing first call
  var idle = true

  private def sorted(query: QueryDTO): QueryDTO = query.copy(
    colSettings = columns.now.map(c => (c.colIndex, c.sortDir, c.sortPos)))

  private def setColumns(query: QueryDTO): Unit = {
    val colSettings = query.colSettings.map(cs => cs._1 -> cs).toMap
    columns() = columns.now.map { column =>
      val (_, sort, sortPos) = colSettings(column.colIndex)
      column.copy(sortDir = sort, sortPos = sortPos)
    }
  }

  def upsertQuery(query: QueryDTO): Unit = Rx {
    if (idle) {
      idle = false
      // Re-draw query list
      curMolecule.recalc()
      queryWire().upsertQuery(db, query).call().foreach {
        case Right("Successfully inserted query") =>
          savedQueries = savedQueries :+ query
          setColumns(query)
          idle = true

        case Right("Successfully updated query") =>
          // Keep saved and recent queries in sync
          val m = query.molecule
          savedQueries = savedQueries.map {
            case QueryDTO(`m`, _, _, _, _, _, _) => query
            case q                               => q
          }
          recentQueries = recentQueries.map {
            case QueryDTO(`m`, _, _, _, _, _, _) => query
            case q                               => q
          }
          setColumns(query)
          idle = true

        case Right(msg) =>
          window.alert(s"Unexpected successful query upsertion: $msg")
          idle = true

        case Left(error) =>
          window.alert(s"Error upserting query: $error")
          idle = true
      }
    }
  }

  protected val upsertQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => upsertQuery(sorted(query))


  protected val retractQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => Rx {
      if (idle) {
        idle = false
        queryWire().retractQuery(db, query).call().foreach {
          case Right(_)    =>
            savedQueries = savedQueries.filterNot(_.molecule == query.molecule)
            idle = true
          case Left(error) =>
            window.alert(s"Error retracting query: $error")
            idle = true
        }
      }
    }

  def updateQuery(updatedQuery: QueryDTO): Rx.Dynamic[Unit] = Rx {
    if (idle) {
      idle = false
      queryWire().updateQuery(db, updatedQuery).call().foreach {
        case Right(_) =>
          // Keep saved and recent queries in sync
          val m = updatedQuery.molecule
          savedQueries = savedQueries.map {
            case QueryDTO(`m`, _, _, _, _, _, _) => updatedQuery
            case q                               => q
          }
          recentQueries = recentQueries.map {
            case QueryDTO(`m`, _, _, _, _, _, _) => updatedQuery
            case q                               => q
          }
          idle = true

        case Left(error) =>
          window.alert(s"Error updating query: $error")
          idle = true
      }
    }
  }

  protected val favoriteQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => updateQuery(query.copy(isFavorite = true))


  protected val unfavoriteQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => updateQuery(query.copy(isFavorite = false))


  def useQuery(query: QueryDTO): Rx.Dynamic[Unit] = Rx {
    if (idle) {
      idle = false
      Molecule2Model(query.molecule) match {
        case Right(elements) =>
          //          println("---- use query")
          modelElements() = elements
          setColumns(query)
          idle = true
        case Left(err)       =>
          window.alert(s"Error using query: $err")
          idle = true
      }
    }
  }

  protected val useQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => useQuery(query)

  //  def toggleViews(): Rx.Dynamic[Unit] = Rx {
  //    if (idle) {
  //      idle = false
  //      showGrouped() = !showGrouped.now
  //      groupedCols.recalc()
  //    }
  //  }

  def toggleShowGrouped(): Rx.Dynamic[Unit] = Rx {
    if (idle) {
      idle = false
//      println("toggleShowGrouped")
      showGrouped = !showGrouped
      groupedCols.recalc()
      //      val updatedQuery = queryCache.
      //      queryWire().updateQuery(db, updatedQuery).call().foreach {
      //        case Right(_) =>
      ////          // Keep saved and recent queries in sync
      ////          val m = updatedQuery.molecule
      ////          savedQueries() = savedQueries.now.map {
      ////            case QueryData(`m`, _, _, _, _, _, _) => updatedQuery
      ////            case q                                => q
      ////          }
      ////          recentQueries() = recentQueries.now.map {
      ////            case QueryData(`m`, _, _, _, _, _, _) => updatedQuery
      ////            case q                                => q
      ////          }
      //          idle = true
      //
      //        case Left(error) =>
      //          window.alert(s"Error toggling showGrouped status: $error")
      //          idle = true
      //      }
    }
  }

  def getCb(id: String): HTMLInputElement =
    document.getElementById(id).asInstanceOf[HTMLInputElement]

  def toggleGrouped(colIndex: Int): Rx.Dynamic[Unit] = Rx {
    if (idle) {
      idle = false
      val cbAll = getCb("checkbox-grouped--1")
      val cb    = getCb("checkbox-grouped-" + colIndex)

      if (groupedCols.now.contains(colIndex)) {
        // on -> off
        if (groupedCols.now.size == 1) {
          cbAll.checked = false
          showGrouped = false
        }
        if (cb.checked) cb.checked = false
        groupedCols() = groupedCols.now.filterNot(_ == colIndex)

      } else {
        // off -> on
        if (groupedCols.now.isEmpty) {
          cbAll.checked = true
          showGrouped = true
        }
        if (!cb.checked) cb.checked = true
        groupedCols() = groupedCols.now + colIndex
      }


      //      queryWire().updateQuery(db, updatedQuery).call().foreach {
      //        case Right(_) =>
      //          // Keep saved and recent queries in sync
      //          val m = updatedQuery.molecule
      //          savedQueries() = savedQueries.now.map {
      //            case QueryData(`m`, _, _, _, _, _, _) => updatedQuery
      //            case q                                => q
      //          }
      //          recentQueries() = recentQueries.now.map {
      //            case QueryData(`m`, _, _, _, _, _, _) => updatedQuery
      //            case q                                => q
      //          }
      //          idle = true
      //
      //        case Left(error) =>
      //          window.alert(s"Error updating query: $error")
      //          idle = true
      //      }

      //      Molecule2Model(query.molecule) match {
      //        case Right(elements) =>
      //          //          showGrouped() = query.showGrouped
      //          //          groupedCols() = query.groupedCols
      //          modelElements() = elements
      //          setColumns(query)
      //          idle = true
      //        case Left(err)       =>
      //          window.alert(s"Error using query: $err")
      //          idle = true
      //      }
    }
  }
}
