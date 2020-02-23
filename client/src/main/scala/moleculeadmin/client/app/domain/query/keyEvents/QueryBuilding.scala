package moleculeadmin.client.app.domain.query.keyEvents

import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import rx.{Ctx, Rx}


trait QueryBuilding {


  def queryBuilderOpen: Boolean = querySelection.now.nonEmpty

  def saveSelection(implicit ctx: Ctx.Owner): Unit = {
    (new Callbacks).saveSetting("querySelection" -> querySelection.now)
    (new Callbacks).saveSetting("queryBaseSelection" -> queryBaseSelection)
  }

  def queryBuilder(key: String)(implicit ctx: Ctx.Owner): Unit = {
    key match {
      case "m" if modelElements.now.nonEmpty => toggleMinimize

      // All attributes
      case "a" if querySelection.now != "a" =>
        queryBaseSelection = "a"
        querySelection() = "a"

      // Todo: we use 'v' for Views - this is hackish...
      // Attributes with values
      case "w" if querySelection.now != "v" =>
        queryBaseSelection = "v"
        querySelection() = "v"

      // Attributes without rel attributes
      case "r" if querySelection.now != "r" =>
        queryBaseSelection = "r"
        querySelection() = "r"
      case _                                => ()
    }
    saveSelection
  }

  def toggleQueryBuilder(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] = Rx {
    if (querySelection.now == "") {
      if (queryMinimized) {
        querySelection() = "m"
      } else {
        queryBaseSelection match {
          case "a" => querySelection() = "a"
          case "v" => querySelection() = "v"
          case "r" => querySelection() = "r"
        }
      }
    } else {
      querySelection() = ""
    }
    saveSelection
  }

  def toggleMinimize(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] = Rx {
    if (querySelection.now == "m") {
      queryMinimized = false
      queryBaseSelection match {
        case "a" => querySelection() = "a"
        case "v" => querySelection() = "v"
        case "r" => querySelection() = "r"
      }
    } else {
      queryMinimized = true
      querySelection() = "m"
    }
    saveSelection
  }


  def toggleAttrSelectionA(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] = Rx {
    queryBaseSelection = "a"
    querySelection() = "a"
    saveSelection
  }

  def toggleAttrSelectionR(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] = Rx {
    queryBaseSelection = "r"
    querySelection() = "r"
    saveSelection
  }

  def toggleAttrSelectionV(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] = Rx {
    queryBaseSelection = "v"
    querySelection() = "v"
    saveSelection
  }


}
