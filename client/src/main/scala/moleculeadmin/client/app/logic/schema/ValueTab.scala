package moleculeadmin.client.app.logic.schema
import autowire._
import boopickle.Default._
import util.client.rx.RxBindings
import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.schemaWire
import moleculeadmin.shared.api.BaseApi
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.util.HelpersAdmin
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.html.Div
import rx.{Ctx, Rx, Var}
import scalatags.JsDom
import scalatags.JsDom.all.{textAlign, _}
import scala.concurrent.ExecutionContext.Implicits.global


case class ValueTab(db: String, flatSchema0: FlatSchema)(implicit val ctx: Ctx.Owner)
  extends RxBindings with BaseApi with HelpersAdmin with AppElements {

  type keepBooPickleImport_ValueTab = PickleState

  val metaSchemaAll = Var[FlatSchema](Nil)
  val metaSchema    = Var[FlatSchema](Nil)
  val showAll       = Var[Boolean](true)
  val sort          = Var[Int](0)
  val processing    = Var[Boolean](false)


  init(flatSchema0)

  def init(flatSchema1: FlatSchema): Rx.Dynamic[Unit] = {
    Rx {
      metaSchemaAll() = flatSchema1
      metaSchema() = if (showAll()) flatSchema1 else {
        sort() match {
          case 0 => flatSchema1.filter(_.entityCount$.nonEmpty)
          case 1 => flatSchema1.filter(_.entityCount$.nonEmpty).sortBy(_.entityCount$.get).reverse
          case 2 => flatSchema1.filter(_.entityCount$.nonEmpty).sortBy(_.entityCount$.get)
        }
      }
    }
  }


  def render2: Rx.Dynamic[JsDom.TypedTag[Div]] = Rx {
    div(
      marginTop := 20,
      marginLeft := 40,
      p("Schema with counts of asserted attributes"),
      Rx(
        div(
          _btn2(
            "Update value counts", "update", "", { () =>
              processing() = true
              schemaWire().updateSchemaCounts(db).call().foreach { _ => }
            }
          ),
          if (processing()) span(_sync(), " Updating... See terminal for progress and refresh when process if finished.") else ()
        )
      ),
      p(),
      Rx {
        val partitions = metaSchema().map(_.part).distinct.length
        val namespaces = metaSchema().map(_.ns).distinct.length
        val allAttributes = metaSchemaAll().length
        val attributes = metaSchema().length
        val withValue = metaSchemaAll().count(_.entityCount$.nonEmpty)
        val withValuePct = withValue * 100 / allAttributes

        table(cls := "table", width.auto)(
          thead(
            tr(
              th(),
              th(partitions, verticalAlign.top),
              th(namespaces, verticalAlign.top),
              th(
                colspan := 10,
                verticalAlign.top,
                attributes + (
                  if (showAll())
                    s" ($withValue / $withValuePct% with value)"
                  else
                    s" / $allAttributes ($withValuePct%)"
                  )
              ),
              th(
                label(
                  style := "cursor: pointer;",
                  textAlign.right,
                  input(
                    //                  id := "toggleTree",
                    tpe := "checkbox",
                    style := "cursor: pointer;",
                    onclick := { () => showAll() = !showAll.now },
                    if (showAll()) checked := true else ()
                  ),
                  " Show all"
                )
              )
            ),
            tr(
              th(),
              th("Partition"),
              th("Namespace"),
              th("Attribute"),
              th("Type"),
              th("Cardinality"),
              th(width := "30px"),
              th(textAlign.right,
                if (showAll()) "Entities" else {
                  a(
                    "Entities",
                    href := "#",
                    onclick := { () =>
                      sort() = sort() match {
                        case 0 => 1
                        case 1 => 2
                        case 2 => 0
                      }
                    }
                  )
                }
              ),
              th(width := "30px"),
              th("Values", textAlign.right),
              th(width := "30px"),
              th("Ref label", textAlign.center),
              th(width := "30px"),
              th("Top 5 values")
            )
          ),
          Rx(
            tbody(
              for (FlatAttr(i, part, _, nsAlias, _, _, attr, card, tpe0, _, ref, _, _, _, count0, distinctCount0, descrAttr0, topValues0) <- metaSchema()) yield {
                val tpe = ref match {
                  case Some(refNs) if refNs.contains('_') => "ref[" + refNs + "]"
                  case Some(refNs)                        => "ref[" + refNs.capitalize + "]"
                  case None                               => tpe0
                }
                val count = if (count0.isEmpty) "" else thousands(count0.get)
                val distinctCount = if (distinctCount0.isEmpty) "" else thousands(distinctCount0.get)
                val topValues = if (topValues0.isEmpty)
                  div("")
                else if (topValues0.head.label$.nonEmpty) {
                  div(
                    topValues0.sortBy(_.entityCount).reverse.take(5).map { case TopValue(c, v, label) =>
                      div(label.get, span(thousands(c), float.right, marginLeft := "10px", color := "grey", fontFamily := "courier", whiteSpace.nowrap))
                    }
                  )
                } else {
                  div(
                    topValues0.sortBy(_.entityCount).reverse.take(5).map { case TopValue(c, v, _) =>
                      div(v, span(thousands(c), float.right, color := "grey", marginLeft := "10px", fontFamily := "courier", whiteSpace.nowrap))
                    }
                  )
                }

                val descrAttr = descrAttr0.getOrElse("")
                val attrSelector = if (
                  ref.nonEmpty && (
                    count0.isEmpty && distinctCount0.isEmpty
                      ||
                      distinctCount0.isDefined && distinctCount0.get <= 25
                      ||
                      count0.isDefined && distinctCount0.isDefined &&
                        count0.get > 0 &&
                        count0.get / distinctCount0.get >= 2 &&
                        count0.get != distinctCount0.get
                    )

                ) {
                  val attrsInRefNs: Seq[String] = metaSchema().filter(at => at.nsFull == ref.get).map(_.attr)
                  val sel = select(
                    option("Select attr...", value := ""),
                    for (refNsAttr <- attrsInRefNs) yield {
                      option(
                        refNsAttr,
                        value := refNsAttr,
                        if (refNsAttr == descrAttr) selected := true else ()
                      )
                    }
                  ).render
                  sel.onchange = (e: Event) => {
                    val descrAttr = if (sel.value.nonEmpty) Some(sel.value) else None
                    schemaWire().updateDescrAttr(db, part, nsAlias, attr, descrAttr).call().foreach { _ =>
                      dom.document.getElementById("topValues" + i).innerHTML = "Please update value counts..."
                    }
                  }
                  sel
                } else div().render

                tr(
                  if (count.nonEmpty) backgroundColor := "#6fff952e" else (),
                  td(i),
                  td(fontWeight.bold, part),
                  td(fontWeight.bold, nsAlias.capitalize),
                  td(fontWeight.bold, attr),
                  td(tpe),
                  td(card, textAlign.center),
                  td(),
                  td(count, textAlign.right, fontFamily := "courier", whiteSpace.nowrap),
                  td(),
                  td(distinctCount, textAlign.right, fontFamily := "courier", whiteSpace.nowrap),
                  td(),
                  td(attrSelector),
                  td(),
                  td(topValues, id := "topValues" + i)
                )
              }
            )
          )
        )
      }
    )
  }
}
