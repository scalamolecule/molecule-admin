package moleculeadmin.client.app.domain.query
import boopickle.Default._
import molecule.ast.model.Model
import molecule.transform.Model2Query
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.snippet.{Datalog, Entity, EntityHistory, Transaction}
import moleculeadmin.client.app.element.query.SnippetElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ops.query.ModelOps
import moleculeadmin.shared.ops.query.builder.TreeOps
import org.scalajs.dom.html.Element
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._


case class SnippetRender(db: String)(implicit val ctx: Ctx.Owner)
  extends Callbacks(db) with RxBindings with SnippetElements with ModelOps with TreeOps {
  type keepBooPickleImport2 = PickleState


  def moleculeSnippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    val lines      = curMolecule.now.split("\n")
    val rows       = lines.length + 2
    val cols       = lines.map(_.length).max + 25
    val alreadyFav = favorites().exists(_.molecule == curMolecule.now)
    _moleculeSnippet(rows, cols, curMolecule.now,
      addFavCallback(curMolecule.now), alreadyFav)
  }

  def favoritesSnippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    _favoritesSnippet(
      favorites().sortBy(_.molecule),
      curMolecule.now,
      useFavCallback,
      retractFavCallback
    )
  }

  def cacheSnippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    // Clear query cache when max rows is changed
    maxRows()
    _cacheSnippet(
      queryCache().map(_.molecule).sorted,
      curMolecule.now,
      favorites().map(_.molecule),
      resetCacheCallback,
      useCachedCallback,
      addFavCallback,
      removeCachedCallback
    )
  }

  def modelSnippet: TypedTag[Element] =
    _codeSnippet("Molecule Model", "scala", Model(modelElements.now).toString())

  def querySnippet: TypedTag[Element] =
    if (emptyNamespaces(modelElements.now).nonEmpty)
      div(
        "To render Molecule Query, please select attr/ref in empty namespaces:",
        ul(for (ns <- emptyNamespaces(modelElements.now)) yield li(ns))
      )
    else
      _codeSnippet("Molecule Query", "scala",
        Model2Query(Model(modelElements.now))._1.toString)

  def columnsSnippet = Rx {
    // Re-draw when columns change
    columns()
    _codeSnippet("Columns", "scala", columns.now.mkString("List(\n  ", ",\n  ", ")"), hljs)
  }

  def tree1Snippet: TypedTag[Element] =
    _codeSnippet("Tree with attr names only", "scala", tree.now.toString)
  def tree2Snippet: TypedTag[Element] =
    _codeSnippet("Tree with attr definitions", "scala", tree.now.code)
  def tree3Snippet: TypedTag[Element] =
    _codeSnippet("Full Tree", "scala", tree.now.code2)


  def rxElement: Rx.Dynamic[TypedTag[Element]] = Rx {
    //    println("SnippetRender...")
    if (modelElements().nonEmpty && showSnippets()) {
      _snippets(
        if (showMolecule()) moleculeSnippet else (),
        if (showFavorites()) favoritesSnippet else (),
        if (showCache()) cacheSnippet else (),
        if (showDatalog()) Datalog() else (),
        if (showTransaction()) Transaction(db).snippet else (),
        if (showEntity()) Entity(db).snippet else (),
        if (showEntityHistory()) EntityHistory(db).snippet else (),

        if (showModel()) modelSnippet else (),
        if (showQuery()) querySnippet else (),
        if (showColumns()) columnsSnippet else (),
        if (showTree1()) tree1Snippet else (),
        if (showTree2()) tree2Snippet else (),
        if (showTree3()) tree3Snippet else ()
      )
    } else span()
  }
}
