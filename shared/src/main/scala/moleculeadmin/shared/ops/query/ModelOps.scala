package moleculeadmin.shared.ops.query

import moleculeadmin.shared.lib.molecule.ast.model._
import moleculeadmin.shared.lib.moleculeExtras.HelpersAdmin


trait ModelOps extends HelpersAdmin {

  def emptyNamespaces(elements: Seq[Element]): Seq[String] = elements.collect {
    case Atom(nsFull, `dummy`, _, _, _, _, _, _) => nsFull
  }

  def nsHasOnlyDummyAttr(elements: Seq[Element]): Boolean = elements.size match {
    case 0 => throw new RuntimeException("Unexpected empty model.")
    case 1 => elements.last match {
      case Generic(_, "e" | "e_", _, _) => true
      case _                            => false
    }
    case _ => elements.last match {
      case Generic(nsFull, "e" | "e_", _, _) => elements.init.last match {
        case Atom(`nsFull`, _, _, _, _, _, _, _) => false // Real attribute in same namespace
        case _                                   => true // Bond, other generic
      }
      case _                                 => false
    }
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
        val value = if(enumPrefix.isEmpty) VarValue else EnumVal
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
