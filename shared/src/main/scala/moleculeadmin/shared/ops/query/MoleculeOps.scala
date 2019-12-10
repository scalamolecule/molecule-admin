package moleculeadmin.shared.ops.query

import molecule.ast.model._
import moleculeadmin.shared.util.HelpersAdmin


trait MoleculeOps extends HelpersAdmin {

  def getPartNs(m: String) = {
    m.substring(0, m.indexOf('.')) match {
      case r"(\w+)${part}_(\w+)$ns" => (part, ns)
      case ns                       => ("db.part/user", ns)
    }
  }
}
