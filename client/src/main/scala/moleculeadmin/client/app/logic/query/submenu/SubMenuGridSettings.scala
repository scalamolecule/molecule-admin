package moleculeadmin.client.app.logic.query.submenu

import moleculeadmin.client.app.html.query.SubMenuElements
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.{Callbacks, KeyEvents}
import rx.{Ctx, Rx}
import scalatags.JsDom.all._


case class SubMenuGridSettings()(implicit val ctx: Ctx.Owner)
  extends SubMenuElements with KeyEvents {

  def render: Frag = Rx {
    //    println("SubMenuNestedGrid...")

    columns()

    if (columns.now.exists(_.sortDir.nonEmpty)) {
      li(
        display.flex,
        paddingLeft := 30,
        _gridSelector,
        levelsOn
      )
    } else li()
  }

  _gridSelector.onchange = _ => {
    gridType() = _gridSelector.value.toInt
    new Callbacks().saveSetting("gridType" -> gridType.now.toString)
    _gridSelector.blur()
  }

  def levelsOn: Frag = Rx {
    gridColIndexes()

    val sortColIndexes = getSortColIndexes(columns.now)
    if (sortColIndexes.isEmpty) span() else
      div(
        cls := "prio",
        sortColIndexes.zipWithIndex.map { case (colIndex, i) =>
          if (gridColIndexes.now contains colIndex) {
            prioOn(i)(
              onclick := { () =>
                gridColIndexes() = gridColIndexes.now.filterNot(_ == colIndex)
                // force new RowBuilderNested and re-rendered body/foot
                cachedGridType = -1
                columns.recalc()
              }
            )
          } else {
            prioOff(i)(
              onclick := { () =>
                gridColIndexes() = gridColIndexes.now :+ colIndex
                // force new RowBuilderNested and re-rendered body/foot
                cachedGridType = -1
                columns.recalc()
              }
            )
          }
        }
      )
  }.render
}