package moleculeadmin.shared.ops.query

import molecule.ast.model._
import moleculeadmin.shared.util.HelpersAdmin


trait ModelOps extends BaseQuery with HelpersAdmin {

  def emptyNamespaces(elements: Seq[Element]): Seq[String] = elements.collect {
    case Atom(nsFull, `dummy`, _, _, _, _, _, _) => nsFull
  }


  def hasIncompleteBranches(elements: Seq[Element]): Boolean = {
    elements.foldLeft(true, false) {
      case ((res, _), Generic(_, "e", _, _)) => (res, true)
      case ((res, gen), _: Generic)          => (res, gen)
      case ((res, gen), _: ReBond)           => (res, gen)
      case ((res, _), _: Bond)               => (res, false)
      case (_, a: Atom) if mandatory(a.attr) => (false, false)
      case ((_, true), _: Atom)              => (false, false)
      case ((res, gen), _)                   => (res, gen)
    }._1
  }

  def toggleEdit(elements: Seq[Element],
                 colIndex: Int,
                 nsFull: String,
                 attr: String): Seq[Element] = {
    elements.foldLeft(0, 0, Seq.empty[Element]) {
      // Toggle off
      case ((0, `colIndex`, es), Atom(`nsFull`, `attr`, _, _, _, _, _, Seq("edit"))) =>
        val nonEditingAtom = es.last match {
          case a@Atom(_, _, _, _, _, _, _, keys) =>
            a.copy(keys = keys.filterNot(_ == "orig"))
        }
        (1, 100, es.init :+ nonEditingAtom)

      // Toggle on
      case ((0, `colIndex`, es), a@Atom(`nsFull`, `attr`, _, _, _, enumPrefix, _, keys))
        if (!keys.contains("orig")) =>
        val value = if (enumPrefix.isEmpty) VarValue else EnumVal
        (1, 100, es :+
          a.copy(keys = "orig" +: a.keys) :+
          a.copy(value = value, keys = Seq("edit")))

      case ((0, `colIndex`, _), Atom(`nsFull`, `attr`, _, _, _, _, _, _)) =>
        throw new IllegalArgumentException(s"Unexpected col index $colIndex for toggling edit")

      // Advance colIndex for Atoms and Generics
      case ((done, i, es), a: Atom)    => (done, i + 1, es :+ a)
      case ((done, i, es), g: Generic) => (done, i + 1, es :+ g)
      case ((done, i, es), e)          => (done, i, es :+ e)
    }._3
  }
}
