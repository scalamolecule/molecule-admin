package moleculeadmin.client.app.domain.query
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.{ColSetting, QueryData}
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.ops.transform.Molecule2Model
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scala.concurrent.ExecutionContext.Implicits.global


class Callbacks(implicit ctx: Ctx.Owner)
  extends RxBindings with ColOps {

  type keepBooPickleImport_Callbacks = PickleState

  // Prevent multiple asynchronous calls to db while processing first call
  var idle = true

  def saveQuery(query: QueryData): Unit = Rx {
    if (idle) {
      idle = false
      queryWire().addQuery(db, query).call().foreach {
        case Right(_)    =>
          savedQueries() = savedQueries.now :+ query
          idle = true
        case Left(error) =>
          window.alert(s"Error adding query: $error")
          idle = true
      }
    }
  }

  protected val saveQueryCallback: QueryData => () => Unit =
    (query: QueryData) => () => saveQuery(query)

  protected val retractQueryCallback: QueryData => () => Unit =
    (query: QueryData) => () => Rx {
      if (idle) {
        idle = false
        queryWire().retractQuery(db, query).call().foreach {
          case Right(_)    =>
            savedQueries() = savedQueries.now
              .filterNot(_.molecule == query.molecule)
            idle = true
          case Left(error) =>
            window.alert(s"Error retracting query: $error")
            idle = true
        }
      }
    }

  def updateQuery(updatedQuery: QueryData): Rx.Dynamic[Unit] = Rx {
    if (idle) {
      idle = false
      queryWire().updateQuery(db, updatedQuery).call().foreach {
        case Right(_)    =>
          val m = updatedQuery.molecule

          // Keep saved and recent queries in sync
          savedQueries() = savedQueries.now.map {
            case QueryData(`m`, _, _, _, _, _, _) => updatedQuery
            case q                                => q
          }
          recentQueries() = recentQueries.now.map {
            case QueryData(`m`, _, _, _, _, _, _) => updatedQuery
            case q                                => q
          }
          idle = true

        case Left(error) =>
          window.alert(s"Error updating query: $error")
          idle = true
      }
    }
  }

  protected val favoriteQueryCallback: QueryData => () => Unit =
    (query: QueryData) => () => updateQuery(query.copy(isFavorite = true))


  protected val unfavoriteQueryCallback: QueryData => () => Unit =
    (query: QueryData) => () => updateQuery(query.copy(isFavorite = false))


  def useQuery(query: QueryData): Rx.Dynamic[Unit] = Rx {
    if (idle) {
      idle = false
      Molecule2Model(query.molecule) match {
        case Right(elements) =>
          showGrouped() = query.showGrouped
          groupedCols() = query.groupedCols
          modelElements() = elements
          val colSettings = query.colSettings.map(cs => cs.colIndex -> cs).toMap

          // Columns have been calculated when model updated
          // Now add sorting settings
          columns() = columns.now.map { column =>
            val ColSetting(colIndex, sort, sortPos) =
              colSettings(column.colIndex)
            column.copy(
              colIndex = colIndex,
              sortDir = sort,
              sortPos = sortPos
            )
          }
          idle = true
        case Left(err)       =>
          window.alert(s"Error using query: $err")
          idle = true
      }
    }
  }

  protected val useQueryCallback: QueryData => () => Unit =
    (query: QueryData) => () => useQuery(query)
}
