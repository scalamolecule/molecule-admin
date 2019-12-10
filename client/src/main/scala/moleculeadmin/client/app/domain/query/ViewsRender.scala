package moleculeadmin.client.app.domain.query
import boopickle.Default._
import molecule.ast.model.Model
import molecule.transform.Model2Query
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.views.{Datalog, Entity, EntityHistory, Transaction}
import moleculeadmin.client.app.element.query.ViewElements
import moleculeadmin.shared.ast.query.QueryData
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


case class ViewsRender(db: String)(implicit val ctx: Ctx.Owner)
  extends Callbacks
    with ViewElements with ModelOps with ColOps with TreeOps {
  type keepBooPickleImport_ViewsRender = PickleState


  def moleculeView: Rx.Dynamic[TypedTag[Element]] = Rx {
    val lines      = curMolecule.now.split("\n")
    val rows       = lines.length + 2
    val cols       = lines.map(_.length).max + 25
//    val alreadyFav = savedQueries().exists(_.molecule == curMolecule.now)
    val alreadyFav = savedQueries.now.exists(_.molecule == curMolecule.now)
    _moleculeView(
      rows,
      cols,
      curMolecule.now,
      saveQueryCallback(QueryData(curMolecule.now, "x", "x", false, false, Nil, Nil)),
      alreadyFav
    )
  }

  //  def queriesView: Rx.Dynamic[TypedTag[Element]] = Rx {
  //    _queriesView(
  //      savedQueries().sortBy(_.molecule),
  //      curMolecule.now,
  //      useQueryCallback,
  //      retractQueryCallback
  //    )
  //  }
  //
  //  def recentMoleculesView: Rx.Dynamic[TypedTag[Element]] = Rx {
  //    // Clear recent molecules when max rows is changed
  //    maxRows()
  //    _recentMoleculesView(
  //      queryCache().map(_.molecule).sorted,
  //      curMolecule.now,
  //      savedQueries().map(_.molecule),
  //      resetRecentMoleculesCallback,
  //      useRecentMoleculeCallback,
  //      saveQueryCallback,
  //      removeRecentMoleculeCallback
  //    )
  //  }

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


  def rxElement: Rx.Dynamic[TypedTag[Element]] = Rx {
    //    println("ViewsRender...")
    // Triggering by columns() ensures that both model and
    // columns have been calculated (in DataTable)
    if (columns().nonEmpty && viewsOn()) {
      _cardsContainer(
        if (viewMolecule()) moleculeView else (),
        //        if (viewQueries()) queriesView else (),
        //        if (viewRecentMolecules()) recentMoleculesView else (),
        if (viewDatalog()) Datalog() else (),
        if (viewTransaction()) Transaction(db).view else (),
        if (viewEntity()) Entity(db).view else (),
        if (viewEntityHistory()) EntityHistory(db).view else (),

        if (viewMoleculeModel()) moleculeModelView else (),
        if (viewMoleculeQuery()) moleculeQueryView else (),
        if (viewColumns()) columnsView else (),
        if (viewTree1()) tree1View else (),
        if (viewTree2()) tree2View else (),
        if (viewTree3()) tree3View else ()
      )
    } else span()
  }
}
