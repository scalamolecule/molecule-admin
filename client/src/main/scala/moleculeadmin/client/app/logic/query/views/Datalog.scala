package moleculeadmin.client.app.logic.query.views
import molecule.ast.model.Model
import molecule.ops.QueryOps._
import molecule.transform.Query2String
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.html.query.ViewElements
import moleculeadmin.shared.ops.query.ModelOps
import scalatags.JsDom.all._


object Datalog extends ModelOps with ViewElements {

  def apply() = {
    if (emptyNamespaces(modelElements.now).nonEmpty) {
      div(
        "To render Datalog query, please select attr/ref in empty namespaces:",
        ul(for (ns <- emptyNamespaces(modelElements.now)) yield li(ns))
      )
    } else {
      val q = molecule.transform.Model2Query(Model(modelElements.now))._1
      _codeView(
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
            q.inputs.zipWithIndex.map(r => s"${r._2 + 1}  ${r._1}")
              .mkString("INPUTS:\n", "\n", "")
          )
        } else ()
      )
    }
  }
}
