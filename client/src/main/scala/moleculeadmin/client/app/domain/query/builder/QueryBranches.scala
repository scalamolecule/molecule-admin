package moleculeadmin.client.app.domain.query.builder
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.app.domain.query.QueryState.{modelElements, nsMap, tree}
import moleculeadmin.client.jsdom.DropdownMenu
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.ast.tree.Tree
import moleculeadmin.shared.ops.query.builder.TreeOps
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.window
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._


case class QueryBranches(selection: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with TreeOps with DropdownMenu with AppElements {

  // Unique index across recursive branches
  var index = 1

  def renderBranches(trees: Seq[Tree]): JsDom.TypedTag[Div] = {
    _rowColAuto(
      Rx(
        trees.zipWithIndex.map { case (Tree(path, attrs0, selAttrs, refs0, selRefs, branches), j) =>
          index = index + j
          val (refAttr, nsFull) = path.last
          val (attrs, refs)     = attrSelection(attrs0, selAttrs, refs0, selection)
          val additivesMap      = getAdditives(selAttrs)

          _row(
            _rowColAuto5(
              _topMenu(
                if (refAttr.isEmpty || refAttr == nsFull.low)
                  _cardHeader(h5(nsFull))
                else
                  _cardHeader(h5(refAttr.capitalize), h6(nsFull)),

                // Attributes section -------------------------------
                for {
                  Attr(i, attr, car, attrType, enums, refNs, options, doc, _, _, _, _, topValues) <- attrs
                } yield {
                  val (attrFull, attrClass, attrValue) = attrFullClass(selAttrs, attr, refNs)
                  val manyAsterisk                     = if (car > 2) " **" else if (car == 2) " *" else ""
                  val additive                         = additivesMap.getOrElse(s"$nsFull-$attr", Nil)

                  if (selRefs.contains(attr)) {
                    _submenu(attrClass)(
                      a(
                        color := "#808080cc",
                        href := "#",
                        attrFull + manyAsterisk,
                        onclick := { () =>
                          window.alert(
                            s"Ref attribute value `$attr` can't be selected when namespace `${attr.capitalize}` is active.\n" +
                              s"Entity id `e` of ns `${attr.capitalize}` can be used instead."
                          )
                        }
                      )
                    )
                  } else {
                    // Render attribute options
                    AttrOptions(
                      i, refAttr, nsFull, attr, car, attrType, attrValue, enums, options, doc,
                      topValues, selAttrs, attrClass, path, manyAsterisk, additive
                    ).render
                  }
                },

                // Refs section ---------------------------------------
                if (refs.isEmpty) () else {
                  _divider +: (
                    for ((refAttr, car, Ns(_, _, refNsFull, _, _, refAttrs)) <- refs) yield {
                      val refAttr1 = refAttr.capitalize + (if (car > 2) " **" else if (car == 2) " *" else "")

                      if (selRefs.contains(refAttr)) {
                        _submenu("attr-refNs")(
                          a(href := "#", refAttr1,
                            onclick := { () =>
                              modelElements() = removeBranch(modelElements.now, path :+ refAttr -> refNsFull)
                            }
                          )
                        )
                      } else {
                        _submenu(
                          a(href := "#", refAttr1,
                            onclick := { () =>
                              modelElements() = addNs(modelElements.now, path, refAttr, refNsFull)
                            }
                          ),
                          _menu(
                            for {
                              Attr(_, attr, _, _, _, _, _, _, _, _, _, _, _) <- refAttrs
                            } yield {
                              li(
                                a(href := "#", attr,
                                  onclick := { () =>
                                    modelElements() = addNs(modelElements.now, path, refAttr, refNsFull, attr)
                                  }
                                )
                              )
                            }
                          )
                        )
                      }
                    }
                    )
                }
              )
            ),

            // Recursively render sub branches
            if (branches.nonEmpty) renderBranches(branches) else ()
          )
        }
      )
    )
  }

  def dynRender: Rx.Dynamic[JsDom.TypedTag[HTMLElement]] = tree.map {
    case Tree(Nil, _, _, _, _, _) => span()
    case topTree                  => renderBranches(Seq(topTree))
  }
}