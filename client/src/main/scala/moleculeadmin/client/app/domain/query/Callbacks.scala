package moleculeadmin.client.app.domain.query
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.QueryClient.getCols
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.{Col, ColSetting, SavedQuery, QueryCache}
import moleculeadmin.shared.ops.transform.Molecule2Model
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scala.concurrent.ExecutionContext.Implicits.global


class Callbacks(db: String)(implicit ctx: Ctx.Owner) extends RxBindings {

  type keepBooPickleImport_Callbacks = PickleState

  // Prevent multiple asynchronous calls to db while processing first call
  var idle = true


  // Recent molecules ----------------------------------------------------------

  protected val resetRecentMoleculesCallback: () => Unit =
    () => Rx {
      queryCache() = queryCache.now.take(1)
      modelElements.recalc()
    }

  protected val useRecentMoleculeCallback: String => () => Unit =
    (molecule1: String) => () => Rx {
      modelElements() = queryCache.now
        .find(_.molecule == molecule1).get.modelElements
    }

  protected val removeRecentMoleculeCallback: String => () => Unit =
    (molecule1: String) => () => Rx {
      queryCache() = queryCache.now.filterNot(_.molecule == molecule1)
    }


  // Saved Queries ----------------------------------------------------------

  def colSettings(cols: Seq[Col]): Seq[ColSetting] =
    cols.map(c => ColSetting(
      c.colIndex, c.attrExpr, c.sortDir, c.sortPos, c.filterExpr
    ))

  def getQuery(molecule: String): SavedQuery =
    queryCache.now.find(_.molecule == molecule) match {
      case Some(QueryCache(_, _, _, _, cols, _, _, _, showGrouped, groupedCols)) =>
        SavedQuery(molecule, colSettings(cols), showGrouped, groupedCols)
      case None                                                                  =>
        throw new RuntimeException(
          s"Unexpectedly didn't find query `$molecule` in cache"
        )
    }

  protected val saveQueryCallback   : String => () => Unit =
    (molecule1: String) => () => Rx {
      if (idle) {
        idle = false
        val query = getQuery(molecule1)
        queryWire().addQuery(db, query)
          .call().foreach {
          case Left(error) =>
            window.alert(s"Error adding query: $error")
            idle = true
          case Right(_)    =>
            savedQueries() = savedQueries.now :+ query
            idle = true
        }
      }
    }
  protected val retractQueryCallback: String => () => Unit =
    (molecule1: String) => () => Rx {
      if (idle) {
        idle = false
        queryWire().retractQuery(db, molecule1).call().foreach {
          case Left(error) =>
            window.alert(s"Error retracting query: $error")
            idle = true
          case Right(_)    =>
            savedQueries() = savedQueries.now.filterNot(_.molecule == molecule1)
            idle = true
        }
      }
    }

  def useQuery(query: SavedQuery): Rx.Dynamic[Unit] = Rx {
    if (idle) {
      idle = false
      Molecule2Model(query.molecule) match {
        case Left(err)       =>
          window.alert(s"Error using query: $err")
          idle = true
        case Right(elements) =>
//          showGrouped() = query.showGrouped
//          groupedCols() = query.groupedCols
          modelElements() = elements
          val colSettings = query.colSettings.map(cs => cs.colIndex -> cs).toMap
          columns() = columns.now.map { column =>
            val ColSetting(colIndex, expr, sort, sortPos, _) =
              colSettings(column.colIndex)
            column.copy(
              colIndex = colIndex,
              attrExpr = expr,
              sortDir = sort,
              sortPos = sortPos
            )
          }
          idle = true
      }
    }
  }

  protected val useQueryCallback: SavedQuery => () => Unit =
    (savedQuery: SavedQuery) => () => useQuery(savedQuery)
}
