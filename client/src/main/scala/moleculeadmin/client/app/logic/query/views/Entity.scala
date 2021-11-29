package moleculeadmin.client.app.logic.query.views

import autowire._
import boopickle.Default._
import molecule.util.RegexMatching
import moleculeadmin.client.app.logic.query.QueryState.{curEntity, _}
import moleculeadmin.client.app.html.query.SubMenuElements
import moleculeadmin.client.queryWireAjax
import org.scalajs.dom.html.{Anchor, Element, Select, Span, Table}
import org.scalajs.dom.{document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global

case class Entity()(implicit ctx: Ctx.Owner) extends Base
  with SubMenuElements
  with RegexMatching {

  def view: Element = _entityView(
    entityLevelSelector,
    showBackRefLink,
    entityBackRefs,
    entityData
  ).render


  val showBackRefLink: Rx.Dynamic[TypedTag[Anchor]] = Rx {
    // update with curEntity recalc
    curEntity()

    val toggleBackRefs = { () =>
      showBackRefs = !showBackRefs
      curEntity.recalc()
      saveSetting("showBackRefs" -> showBackRefs.toString)
    }

    _entityShowBackRefs(
      toggleBackRefs,
      if (showBackRefs) "hide" else "show",
    )
  }


  val entityLevelSelector: Rx.Dynamic[Select] = Rx {
    val setLevel = { () =>
      entityLevels = entityLevelSelector.now.value.head.toString.toInt
      curEntity.recalc()
      saveSetting("entityLevels" -> entityLevels.toString)
    }

    val options = Seq(
      option("1 level", if (entityLevels == 1) selected := true else ()),
      option("2 levels", if (entityLevels == 2) selected := true else ()),
      option("3 levels", if (entityLevels == 3) selected := true else ()),
      option("4 levels", if (entityLevels == 4) selected := true else ()),
      option("5 levels", if (entityLevels == 5) selected := true else ()),
    )

    _entityLevelsSelector(
      options,
      setLevel
    )
  }

  val entityBackRefs: Rx.Dynamic[Span] = Rx {
    curEntity()
    if (showBackRefs) {
      val backRefsSpan = _entityBackRefs
      curEntity.now match {
        case 0 =>
          backRefsSpan.appendChild(div("...").render)
          backRefsSpan.appendChild(_backRefSeparator)

        case eid => {
          // Placeholder while we get data from db (to avoid flicker)
          backRefsSpan.appendChild(div(raw("&nbsp;")).render)
          backRefsSpan.appendChild(_backRefSeparator)

          queryWireAjax().getBackRefsData(db, eid).call().foreach {
            case Right(backRefs) =>
              backRefsSpan.innerHTML = ""
              if (backRefs.isEmpty) {
                backRefsSpan.appendChild(div("No back refs").render)
                backRefsSpan.appendChild(_backRefSeparator)
              } else {
                backRefs.foreach {
                  case (backRefAttr, count) =>
                    val nsAttr        = backRefAttr.tail.split('/').toList
                    val (ns, refAttr) = (nsAttr.head, nsAttr(1))
                    val url           =
                      window.location.protocol + "//" +
                        window.location.host +
                        window.location.pathname +
                        "?db=" + db +
                        "&m=" + s"$ns.e.$refAttr($eid)"

                    backRefsSpan.appendChild(
                      _backRefLink(backRefAttr, count, url)
                    )
                }
                backRefsSpan.appendChild(_backRefSeparator)
              }

            case Left(err) =>
              backRefsSpan.appendChild(
                div("Error retrieving back refs: " + err).render
              )
              backRefsSpan.appendChild(_backRefSeparator)
          }
        }
      }
      backRefsSpan
    } else {
      span().render
    }
  }

  val entityData: Rx.Dynamic[TypedTag[Table]] = Rx {
    curEntity() match {
      case 0   => // no entity id marked
      case eid =>
        val view = document.getElementById("entityViewTable")
        if (view == null) {
          // Deselect curEntity (and re-render) if entity view is not rendered yet
          curEntity() = 0L
        } else {
          addEntityRows("entityViewTable", eid, true, 1)
        }
    }
    _entityDatoms("Point on entity id...")
  }

}
