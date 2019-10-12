package moleculeadmin.client.app.domain.query
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.{Col, ColSetting, Favorite, QueryCache}
import moleculeadmin.shared.ops.transform.Molecule2Model
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scala.concurrent.ExecutionContext.Implicits.global


class Callbacks(db: String)(implicit ctx: Ctx.Owner) extends RxBindings {

  type keepBooPickleImport = PickleState

  // Prevent multiple asynchronous calls to db while processing first call
  var idle = true


  // Cache ----------------------------------------------------------

  protected val resetCacheCallback: () => Unit =
    () => Rx {
      queryCache() = queryCache.now.take(1)
      modelElements.recalc()
    }

  protected val useCachedCallback: String => () => Unit =
    (molecule1: String) => () => Rx {
      modelElements() = queryCache.now.find(_.molecule == molecule1).get.modelElements
    }

  protected val removeCachedCallback: String => () => Unit =
    (molecule1: String) => () => Rx {
      queryCache() = queryCache.now.filterNot(_.molecule == molecule1)
    }


  // Favorites ----------------------------------------------------------

  def colSettings(columns: Seq[Col]): Seq[ColSetting] =
    columns.map(c => ColSetting(c.colIndex, c.attrExpr, c.sortDir, c.sortPos))

  def getFavorite(molecule: String): Favorite =
    queryCache.now.find(_.molecule == molecule) match {
      case Some(QueryCache(_, _, _, _, cols, _, _, _)) => Favorite(molecule, colSettings(cols))
      case None                                  =>
        throw new RuntimeException(s"Unexpectedly didn't find molecule `$molecule` in cache")
    }

  protected val addFavCallback    : String => () => Unit =
    (molecule1: String) => () => Rx {
      if (idle) {
        idle = false
        val favorite = getFavorite(molecule1)
        queryWire().addFavorite(db, favorite).call().foreach {
          case Left(error) =>
            window.alert(s"Error adding favorite: $error")
            idle = true
          case Right(_)    =>
            favorites() = favorites.now :+ favorite
            idle = true
        }
      }
    }
  protected val retractFavCallback: String => () => Unit =
    (molecule1: String) => () => Rx {
      if (idle) {
        idle = false
        queryWire().retractFavorite(db, molecule1).call().foreach {
          case Left(error) =>
            window.alert(s"Error retracting favorite: $error")
            idle = true
          case Right(_)    =>
            favorites() = favorites.now.filterNot(_.molecule == molecule1)
            idle = true
        }
      }
    }

  def useFavorite(favorite: Favorite) = Rx {
    if (idle) {
      idle = false
      Molecule2Model(favorite.molecule) match {
        case Left(err)       =>
          window.alert(s"Error using favorite: $err")
          idle = true
        case Right(elements) =>
          modelElements() = elements
          val colSettings = favorite.colSettings.map(cs => cs.index -> cs).toMap
          columns() = columns.now.map { column =>
            val ColSetting(index, expr, sort, sortPos) = colSettings(column.colIndex)
            column.copy(colIndex = index, attrExpr = expr, sortDir = sort, sortPos = sortPos)
          }
          idle = true
      }
    }
  }

  protected val useFavCallback: Favorite => () => Unit =
    (favorite: Favorite) => () => useFavorite(favorite)
}
