package moleculeadmin.client.app.domain.query.snippet
import molecule.ast.model.Model
import molecule.ops.QueryOps._
import molecule.transform.Query2String
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.SnippetElements
import moleculeadmin.shared.ops.query.ModelOps
import scalatags.JsDom.all._


object Datalog extends ModelOps with SnippetElements {

  def apply() = {
    if (emptyNamespaces(modelElements.now).nonEmpty) {
      div(
        "To render Datalog query, please select attr/ref in empty namespaces:",
        ul(for (ns <- emptyNamespaces(modelElements.now)) yield li(ns))
      )
    } else {
      val q = molecule.transform.Model2Query(Model(modelElements.now))._1
      _codeSnippet(
        "Datalog",
        "",
        molecule.transform.Query2String(q).multiLine(50),
        if (q.i.rules.nonEmpty) {
          div(
            p(),
            q.i.rules.map(Query2String(q).p(_)).mkString("RULES:\n[", "\n ", "]")
          )
        } else (),
        if (q.i.inputs.nonEmpty) {
          div(
            p(),
            q.inputs.zipWithIndex.map(r => (r._2 + 1) + "  " + r._1)
              .mkString("INPUTS:\n", "\n", "")
          )
        } else ()
      )
    }
  }
}
