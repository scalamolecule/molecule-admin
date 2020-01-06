package moleculeadmin.shared.ops.query
import molecule.ast.model._
import moleculeadmin.shared.util.HelpersAdmin


trait BaseQuery extends HelpersAdmin {


  def isNumber(attrType: String) = Seq("Int", "Long", "Float", "Double", "BigInt", "BigDecimal").contains(attrType)

  def isolateBranch(model: Seq[Element], targetPath: Seq[(String, String)]): (Seq[Element], Seq[Element], Seq[Element]) = {
    val (_, before1, branch1, after1) = model.foldLeft(
      Seq.empty[(String, String)], // current path
      Seq.empty[Element], // before
      Seq.empty[Element], // branch
      Seq.empty[Element] // after
    ) {
      // first
      case ((Nil, _, _, _), e) if targetPath.size == 1 =>
        (targetPath, Nil, Seq(e), Nil)
      case ((Nil, _, _, _), a: GenericAtom)            =>
        (Seq("" -> a.nsFull), Seq(a), Nil, Nil)
      case ((Nil, _, _, _), b: Bond)                   =>
        (Seq("" -> b.nsFull, b.refAttr -> b.refNs), Seq(b), Nil, Nil)

      // branch
      case ((path, before, branch, after), b: Bond) if path.startsWith(targetPath)   =>
        (path :+ b.refAttr -> b.refNs, before, branch :+ b, after)
      case ((path, before, branch, after), r: ReBond) if path.startsWith(targetPath) =>
        val path1 = if (path.size == 1) path else path.init
        (path1, before, branch :+ r, after)
      case ((path, before, branch, after), e) if path.startsWith(targetPath)         =>
        (path, before, branch :+ e, after)

      // before
      case ((path, before, Nil, _), b: Bond)   =>
        (path :+ b.refAttr -> b.refNs, before :+ b, Nil, Nil)
      case ((path, before, Nil, _), r: ReBond) =>
        (path.init, before :+ r, Nil, Nil)
      case ((path, before, Nil, _), e)         =>
        (path, before :+ e, Nil, Nil)

      // after
      case ((path, before, branch, after), e) =>
        (path, before, branch, after :+ e)
    }

    //    debugBeforeAfter(before1, branch1, after1)
    (before1, branch1, after1)
  }

  def isolateAttr(branch: Seq[Element], nsFull: String, attr: String): (Seq[Element], Seq[Element], Seq[Element]) = {
    val (_, prev, cur, sub) = branch.foldLeft(
      0, // processed
      Seq.empty[Element],
      Seq.empty[Element],
      Seq.empty[Element]
    ) {
      case ((0, _, _, _), Atom(_, `dummy`, _, _, _, _, _, _))                                           => (2, Nil, Nil, Nil)
      case ((0, prev, cur, sub), g@Generic(`nsFull`, "e" | "e_", _, _)) if clean(attr) == "e"           => (2, prev, cur :+ g, sub)
      case ((0 | 1, prev, cur, sub), a@Atom(`nsFull`, attr1, _, _, _, _, _, _)) if clean(attr1) == attr => (1, prev, cur :+ a, sub)
      case ((0, prev, _, _), b@Bond(`nsFull`, _, _, _, _))                                              => (2, prev, Nil, Seq(b))
      case ((1, prev, cur, sub), g@Generic(`nsFull`, "e" | "e_", _, _))                                 => (2, prev, cur, sub :+ g)
      case ((1, prev, cur, sub), g@Generic(`nsFull`, _, _, _))                                          => (1, prev, cur :+ g, sub)
      case ((0, prev, cur, sub), e)                                                                     => (0, prev :+ e, cur, sub)
      case ((1 | 2, prev, cur, sub), e)                                                                 => (2, prev, cur, sub :+ e)
    }
    (prev, cur, sub)
  }
}
