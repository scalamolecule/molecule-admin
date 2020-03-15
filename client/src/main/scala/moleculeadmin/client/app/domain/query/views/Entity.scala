package moleculeadmin.client.app.domain.query.views

import autowire._
import boopickle.Default._
import molecule.util.RegexMatching
import moleculeadmin.client.app.domain.query.QueryState.{curEntity, _}
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.client.autowire.queryWire
import org.scalajs.dom.html.{Anchor, Element, Span, Table}
import org.scalajs.dom.{document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global

case class Entity()(implicit ctx: Ctx.Owner) extends Base
  with SubMenuElements
  with RegexMatching {

  type keepBooPickleImport_Entity = PickleState

  val showBackRefLinks: Rx.Dynamic[TypedTag[Anchor]] = Rx {
    // update with curEntity recalc
    curEntity()

    val backRefsToggler = { () =>
      showEntityViewBackRefs = !showEntityViewBackRefs
      curEntity.recalc()
      saveSetting("showEntityViewBackRefs" -> showEntityViewBackRefs.toString)
    }

    _entityShowBackRefs(
      backRefsToggler,
      if (showEntityViewBackRefs) "hide" else "show",
    )
  }

  val entityBackRefs: Rx.Dynamic[Span] = Rx {
    curEntity()
    if (showEntityViewBackRefs) {
      val backRefsSpan = _entityBackRefs
      curEntity.now match {
        case 0 =>
          backRefsSpan.appendChild(div("...").render)
          backRefsSpan.appendChild(_backRefSeparator)

        case eid => {
          // Placeholder while we get data from db (to avoid flicker)
          backRefsSpan.appendChild(div(raw("&nbsp;")).render)
          backRefsSpan.appendChild(_backRefSeparator)

          queryWire().getBackRefsData(db, eid).call().foreach {
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
      case 0 => // no entity id marked yet
      case eid =>
        val view = document.getElementById("entityViewTable")
        if (view == null) {
          // Deselect curEntity (and re-render) if entity view is not rendered yet
          curEntity() = 0L
        } else {
          addEntityRows("entityViewTable", eid, true, 0)
        }
    }
    _entityDatoms("Point on entity id...")
  }


  def view: Element = _entityView(
    showBackRefLinks,
    entityBackRefs,
    entityData
  ).render
}
