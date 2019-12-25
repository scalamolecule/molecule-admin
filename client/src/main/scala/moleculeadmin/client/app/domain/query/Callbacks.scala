package moleculeadmin.client.app.domain.query
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.QueryDTO
import moleculeadmin.shared.ops.query.{ColOps, MoleculeOps}
import moleculeadmin.shared.ops.transform.Molecule2Model
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.{document, window}
import rx.{Ctx, Rx}
import scala.concurrent.ExecutionContext.Implicits.global


class Callbacks(implicit ctx: Ctx.Owner)
  extends RxBindings with MoleculeOps with ColOps {

  type keepBooPickleImport_Callbacks = PickleState

  private def sorted(query: QueryDTO): QueryDTO = query.copy(
    colSettings = columns.now.map(c => (c.colIndex, c.sortDir, c.sortPos)))

  private def setColumns(query: QueryDTO): Unit = {
    val colSettings = query.colSettings.map(cs => cs._1 -> cs).toMap
    columns() = columns.now.map { column =>
      val (_, sort, sortPos) = colSettings(column.colIndex)
      column.copy(sortDir = sort, sortPos = sortPos)
    }
  }

  def saveSettings2(pairs: Seq[(String, String)]): Unit =
    queryWire().saveSettings(pairs).call().foreach {
      case Right(_)  => println(s"Saved settings: " + pairs.mkString(", "))
      case Left(err) => window.alert(err)
    }

  def saveSetting(pair: (String, String)): Unit = saveSettings2(Seq(pair))

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

  def upsertQuery(query: QueryDTO): Unit = Rx {
    queryWire().upsertQuery(db, query).call().foreach {
      case Right("Successfully inserted query") =>
        println("Inserted query: " + query)
        savedQueries = savedQueries :+ query
        setColumns(query)

      case Right("Successfully updated query") =>
        println("Updated query: " + query)
        updateQueryCaches(query)
        setColumns(query)

      case Right(msg) =>
        window.alert(s"Unexpected successful query upsertion: $msg")

      case Left(error) =>
        window.alert(s"Error upserting query: $error")
    }
  }

  private def updateQuery(query: QueryDTO): Rx.Dynamic[Unit] = Rx {
    queryWire().updateQuery(db, query).call().foreach {
      case Right(_) =>
        println("Updated query: " + query)
        updateQueryCaches(query)

      case Left(error) =>
        window.alert(s"Error updating query: $error")
    }
  }

  protected val upsertQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => {
      upsertQuery(sorted(query))
      // Re-draw whole submenu
      curMolecule.recalc()
    }


  protected val retractQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => Rx {
      queryWire().retractQuery(db, query).call().foreach {
        case Right(_) =>
          println("Removed query: " + query)
          savedQueries = savedQueries.filterNot(_.molecule == query.molecule)
          // Re-draw whole submenu
          curMolecule.recalc()

        case Left(error) =>
          window.alert(s"Error retracting query: $error")
      }
    }

  protected val favoriteQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => updateQuery(query.copy(isFavorite = true))


  protected val unfavoriteQueryCallback: QueryDTO => () => Unit =
    (query: QueryDTO) => () => updateQuery(query.copy(isFavorite = false))


  def useQuery(query: QueryDTO): Rx.Dynamic[Unit] = Rx {
    Molecule2Model(query.molecule) match {
      case Right(elements) =>
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

  def getCurQueryDTO: QueryDTO = {
    val (part, ns) = getPartNs(curMolecule.now)
    QueryDTO(
      curMolecule.now,
      part, ns, true,
      showGrouped,
      groupedCols.now,
      colSettings(columns.now)
    )
  }

  def toggleShowGrouped(): Rx.Dynamic[Unit] = Rx {
    showGrouped = !showGrouped
    groupedCols.recalc()
    upsertQuery(getCurQueryDTO.copy(showGrouped = showGrouped))
  }

  def toggleGrouped(colIndex: Int): Rx.Dynamic[Unit] = Rx {
    val cbAll = getCb("grouped-showGrouped")
    val cb    = getCb("grouped-" + colIndex)

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

    upsertQuery(getCurQueryDTO.copy(
      showGrouped = showGrouped,
      groupedCols = groupedCols.now
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
