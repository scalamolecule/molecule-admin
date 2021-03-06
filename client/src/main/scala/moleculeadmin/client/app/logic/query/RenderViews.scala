package moleculeadmin.client.app.logic.query

import molecule.ast.model.Model
import molecule.transform.Model2Query
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.views.{Datalog, Entity, EntityHistory, Transaction}
import moleculeadmin.client.app.html.query.ViewElements
import moleculeadmin.shared.ast.query.QueryDTO
import moleculeadmin.shared.ops.query.ModelOps
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


case class RenderViews()(implicit val ctx: Ctx.Owner)
  extends Callbacks with ViewElements with ModelOps {


  def dynRender: Rx.Dynamic[TypedTag[Element]] = Rx {
    curViews()
    columns()
    if (showViews && curViews.now.nonEmpty && columns.now.nonEmpty) {
      _cardsContainer(
        curViews.now.sorted.collect {
          case "view01_Molecule"      => moleculeView: Frag
          case "view02_Datalog"       => Datalog(): Frag
          case "view03_Entity"        => Entity().view: Frag
          case "view04_EntityHistory" => EntityHistory().view: Frag
          case "view05_Transaction"   => Transaction().view: Frag
          case "view06_Url"           => urlView: Frag
          case "view07_MoleculeModel" => moleculeModelView: Frag
          case "view08_MoleculeQuery" => moleculeQueryView: Frag
          case "view09_Columns"       => columnsView: Frag
          case "view10_Tree1"         => tree1View: Frag
          case "view11_Tree2"         => tree2View: Frag
          case "view12_Tree3"         => tree3View: Frag
        }
      )
    } else span()
  }

  def moleculeView: Rx.Dynamic[TypedTag[Element]] = Rx {
    val lines      = curMolecule.now.split("\n")
    val rows       = lines.length + 2
    val cols       = lines.map(_.length).max + 25
    val alreadyFav = savedQueries.exists(_.molecule == curMolecule.now)
    _moleculeView(
      rows,
      cols,
      curMolecule.now,
      upsertQueryCallback(QueryDTO(curMolecule.now, "x", "x", false, false, Set.empty[Int], Nil)),
      alreadyFav
    )
  }

  def moleculeModelView: TypedTag[Element] =
    _codeView("Molecule Model", "scala", Model(modelElements.now).toString())

  def moleculeQueryView: TypedTag[Element] =
    if (emptyNamespaces(modelElements.now).nonEmpty)
      div(
        "To render Molecule Query, please select attr/ref in empty namespaces:",
        ul(for (ns <- emptyNamespaces(modelElements.now)) yield li(ns))
      )
    else
      _codeView("Molecule Query", "scala",
        Model2Query(Model(modelElements.now))._1.toString)

  def columnsView: Rx.Dynamic[TypedTag[Element]] = Rx {
    _codeView("Columns", "scala",
      columns().mkString("List(\n  ", ",\n  ", ")"), hljs)
  }

  def tree1View: TypedTag[Element] =
    _codeView("Tree with attr names only", "scala", tree.now.toString)
  def tree2View: TypedTag[Element] =
    _codeView("Tree with attr definitions", "scala", tree.now.code)
  def tree3View: TypedTag[Element] =
    _codeView("Full Tree", "scala", tree.now.code2)

  def urlView = Rx {
    curUrl()
    _urlView(curUrl.now)
  }
}
