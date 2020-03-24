package moleculeadmin.client.app.logic.query

import autowire._
import boopickle.Default._
import util.client.rx.RxBindings
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.html.query.SubMenuElements
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.{Col, QueryDTO}
import moleculeadmin.shared.ops.query.{ColOps, MoleculeOps}
import moleculeadmin.shared.ops.transform.Molecule2Model
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{document, window}
import rx.{Ctx, Rx}
import scala.concurrent.ExecutionContext.Implicits.global


class Callbacks(implicit ctx: Ctx.Owner)
  extends RxBindings with MoleculeOps with ColOps with SubMenuElements {

  def saveSettings2(pairs: Seq[(String, String)]): Unit =
    queryWireAjax().saveSettings(pairs).call().foreach {
      case Right(_)  =>
        if (pairs.length == 1) {
          val pair = pairs.head
          println(s"Saved setting: ${pair._1} -> ${pair._2}")
        } else {
          println(s"Saved settings:")
          pairs.foreach { case (k, v) => println(s"  $k -> $v") }
        }
      case Left(err) => window.alert(err)
    }

  def saveSetting(pair: (String, String)): Unit = saveSettings2(Seq(pair))


  // Query list ------------------------------

  private def sorted(query: QueryDTO): QueryDTO = query.copy(
    colSettings = columns.now.map(c => (c.colIndex, c.sortDir, c.sortPos)))

  def setColumns(query: QueryDTO): Unit = {
    val colSettings = query.colSettings.map(cs => cs._1 -> cs).toMap
    //    colSettings foreach println
    //    columns.now foreach println
    columns() = columns.now.map { column =>
      val (_, sort, sortPos) = colSettings(column.colIndex)
      column.copy(sortDir = sort, sortPos = sortPos)
    }
  }

  private def updateQueryCaches(query: QueryDTO) = {
    val m = query.molecule
    savedQueries = savedQueries.map {
      case QueryDTO(`m`, _, _, _, _, _, _) => query
      case q                               => q
    }
    recentQueries = recentQueries.map {
      case QueryDTO(`m`, _, _, _, _, _, _) => query
      case q                               => q
    }
  }

  def upsertQuery(query: QueryDTO, refreshSubmenu: Boolean = false): Unit = Rx {
    queryWireAjax().upsertQuery(db, query).call().foreach {
      case Right("Successfully inserted query") =>
        savedQueries = savedQueries :+ query
        setColumns(query)
        if (refreshSubmenu) {
          curMolecule.recalc()
          renderSubMenu.recalc()
        }

      case Right("Successfully updated query") =>
        updateQueryCaches(query)
        setColumns(query)
        if (refreshSubmenu) {
          curMolecule.recalc()
          renderSubMenu.recalc()
        }

      case Right(msg) =>
        window.alert(s"Unexpected successful query upsertion: $msg")

      case Left(error) =>
        window.alert(s"Error upserting query: $error")
    }
  }

  private def updateQuery(query: QueryDTO): Rx.Dynamic[Unit] = Rx {
    updateQueryCaches(query)
    queryWireAjax().updateQuery(db, query).call().foreach {
      case Right(_)    =>
      case Left(error) => window.alert(s"Error updating query: $error")
    }
  }

  protected val upsertQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => upsertQuery(sorted(query), true)

  protected val retractQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => Rx {
      queryWireAjax().retractQuery(db, query).call().foreach {
        case Right(_)    =>
        case Left(error) => window.alert(s"Error retracting query: $error")
      }
      savedQueries = savedQueries.filterNot(_.molecule == query.molecule)
      updateFavoritesList()
    }

  protected val favoriteQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => {
      updateQuery(query.copy(isFavorite = true))
      updateFavoritesList()
    }


  protected val unfavoriteQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => {
      updateQuery(query.copy(isFavorite = false))
      updateFavoritesList()
    }


  def getFavoriteQueries: Seq[QueryDTO] =
    savedQueries.filter(_.isFavorite).sortBy(_.molecule)

  def newFav: Seq[QueryDTO] =
    if (curMolecule.now.isEmpty) Nil else Seq(curQuery)

  def updateFavoritesList(): Unit = {
    val list = document.getElementById("querylist-favorites")
    list.innerHTML = ""
    _favoriteQueryRows(
      curMolecule.now,
      newFav,
      getFavoriteQueries,
      useQueryCallback,
      upsertQueryCallback,
      unfavoriteQueryCallback
    ).foreach(row => list.appendChild(row.render))
  }


  def useQuery(query: QueryDTO): Rx.Dynamic[Unit] = Rx {
    Molecule2Model(query.molecule) match {
      case Right(elements) =>
        //        println("useQuery " + modelElements.now)
        //        println("useQuery " + elements)
        modelElements() = elements
        setColumns(query)

      case Left(err) =>
        window.alert(s"Error using query: $err")
    }
  }

  protected val useQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => useQuery(query)


  // Grouped ------------------------------

  private def getCb(id: String): HTMLInputElement =
    document.getElementById("checkbox-" + id).asInstanceOf[HTMLInputElement]

  def curQuery: QueryDTO = {
    val (part, ns) = getPartNs(curMolecule.now)
    QueryDTO(
      curMolecule.now,
      part, ns, true,
      showGrouped,
      groupedColIndexes.now,
      colSettings(columns.now)
    )
  }

  def toggleShowGrouped(): Rx.Dynamic[Unit] = Rx {
    showGrouped = !showGrouped
    groupedColIndexes.recalc()
    upsertQuery(curQuery.copy(showGrouped = showGrouped))
  }

  def toggleGrouped(col: Col): Rx.Dynamic[Unit] = Rx {
    val colIndex = col.colIndex
    val cbAll    = getCb("grouped-showGrouped")
    val cb       = getCb("grouped-" + colIndex)

    if (groupedColIndexes.now.contains(colIndex)) {
      // on -> off
      if (groupedColIndexes.now.size == 1) {
        cbAll.checked = false
        showGrouped = false
      }
      if (cb.checked) cb.checked = false
      groupedColIndexes() = groupedColIndexes.now.filterNot(_ == colIndex)

    } else {
      // off -> on
      if (groupedColIndexes.now.isEmpty) {
        cbAll.checked = true
        showGrouped = true
      }
      if (!cb.checked) cb.checked = true
      groupedColIndexes() = groupedColIndexes.now + colIndex
    }

    upsertQuery(curQuery.copy(
      showGrouped = showGrouped,
      groupedColIndexes = groupedColIndexes.now
    ))
  }


  // Views ------------------------------

  def toggleShowViews(): Rx.Dynamic[Unit] = Rx {
    showViews = !showViews
    curViews.recalc()
    saveSetting("showViews" -> showViews.toString)
  }

  def toggleView(view: String): Rx.Dynamic[Unit] = Rx {
    val cbAll = getCb("view-showViews")
    val cb    = getCb("view-" + view)

    if (curViews.now.contains(view)) {
      // on -> off
      if (curViews.now.size == 1) {
        cbAll.checked = false
        showViews = false
      }
      if (cb.checked) cb.checked = false
      curViews() = curViews.now.filterNot(_ == view)

    } else {
      // off -> on
      if (curViews.now.isEmpty) {
        cbAll.checked = true
        showViews = true
      }
      if (!cb.checked) cb.checked = true
      curViews() = curViews.now :+ view
    }

    saveSettings2(Seq(
      "showViews" -> showViews.toString,
      view -> cb.checked.toString
    ))
  }
}