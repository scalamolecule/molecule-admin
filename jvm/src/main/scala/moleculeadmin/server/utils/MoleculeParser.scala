package moleculeadmin.server.utils

import molecule.ast.model._

object MoleculeParser {

  def parse(moleculeStr: String): List[Element] = moleculeStr.split('.').foldLeft("", List.empty[Element]) {
    case ((_, es), e) if e.contains("_") => (e, es)
    case ((nsFull, es), e)               => (nsFull, es :+ Atom(nsFull, e, "", 1, VarValue, None, Nil, Nil))
  }._2
}
