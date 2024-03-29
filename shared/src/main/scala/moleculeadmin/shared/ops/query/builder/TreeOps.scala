package moleculeadmin.shared.ops.query.builder

import molecule.ast.model._
import moleculeadmin.shared.ast.metaSchema.{MetaAttr, MetaNs}
import moleculeadmin.shared.ast.tree.Tree
import moleculeadmin.shared.ops.query.{BaseQuery, DebugBranches}


trait TreeOps extends BaseQuery with DebugBranches {

  private def refs(ns: String)
                  (implicit nsMap: Map[String, MetaNs]): Seq[(String, MetaNs)] = for {
    MetaAttr(_, attr, _, tpe, _, ref, _, _, _, _, _, _, _) <- nsMap(ns).attrs if tpe == "ref"
    ns1@MetaNs(_, _, refFull, _, _, _) <- nsMap.values if refFull == ref.get
  } yield attr -> ns1


  def mkTree(modelTree: Seq[Any])(implicit nsMap: Map[String, MetaNs]): Tree = {

    def traverse(path0: Seq[(String, String)],
                 tree0: Tree,
                 modelTree: Seq[Any]): (Seq[(String, String)], Tree) = {

      val (traversedPath, traversedTree) = modelTree.foldLeft(path0, tree0) {

        case ((prevPath, prevTree@Tree(_, Nil, _, _, _, _)), a: GenericAtom) =>
          val path = if (prevPath.isEmpty) Seq("" -> a.nsFull) else prevPath
          val tree = prevTree.copy(
            path = path,
            attrs = nsMap(a.nsFull).attrs,
            selAttrs = prevTree.selAttrs :+ a,
            refs = refs(a.nsFull)
          )
          (path, tree)

        case ((prevPath, prevTree@Tree(_, Nil, _, _, _, _)), b: Bond) =>
          val tree  = prevTree.copy(
            path = if (prevPath.isEmpty) Seq("" -> b.nsFull) else prevPath,
            attrs = nsMap(b.nsFull).attrs,
            refs = refs(b.nsFull),
            selRefs = prevTree.selRefs :+ b.refAttr
          )
          val path1 = if (prevPath.isEmpty)
            Seq("" -> b.nsFull, b.refAttr -> b.refNs)
          else
            prevPath :+ b.refAttr -> b.refNs
          (path1, tree)

        case ((prevPath, prevTree), a: GenericAtom) =>
          val tree = prevTree.copy(
            selAttrs = prevTree.selAttrs :+ a
          )
          (prevPath, tree)

        case ((prevPath, prevTree), b: Bond) =>
          val tree = prevTree.copy(
            selRefs = prevTree.selRefs :+ b.refAttr
          )
          (prevPath :+ b.refAttr -> b.refNs, tree)

        case ((prevPath, prevTree), r: ReBond) =>
          (prevPath.init, prevTree)

        case ((prevPath, prevTree), branchElements: Seq[_]) =>
          val emptyTree         = Tree(prevPath, Nil, Nil, Nil, Nil, Nil)
          val (path, newBranch) = traverse(prevPath, emptyTree, branchElements)
          val tree              = prevTree.copy(
            branches = prevTree.branches :+ newBranch
          )
          (path, tree)

        case ((path, tree), _) =>
          (path, tree)
      }

      (traversedPath, traversedTree)
    }
    traverse(Nil, Tree(Nil, Nil, Nil, Nil, Nil, Nil), modelTree)._2
  }


  def mkModelTree(elements: Seq[Element]): Seq[Any] = {
    def nest2(e: Element): Seq[Object] = e match {
      case b: Bond => Seq(b, Nil)
      case _       => Seq(e)
    }
    def insertElement2(elements: Seq[Any],
                       e: Element,
                       targetDepth: Int = 0,
                       curDepth: Int = 0): Seq[Any] = elements match {
      case Nil => Seq(e)
      case _   => elements.last match {
        case _: List[_] if targetDepth == curDepth =>
          elements ++ nest2(e)

        case l: List[_] =>
          elements.init :+ insertElement2(l, e, targetDepth, curDepth + 1)

        case _: Bond =>
          elements :+ Seq(e)

        case _: Element =>
          elements ++ nest2(e)

      }
    }
    elements.foldLeft(0, Seq.empty[Any]) {
      case ((_, Nil), b: Bond)   => (1, Seq(b))
      case ((_, Nil), e)         => (0, Seq(e))
      case ((d, acc), b: Bond)   => (d + 1, insertElement2(acc, b, d))
      case ((d, acc), r: ReBond) => (d - 1, insertElement2(acc, r, d))
      case ((d, acc), e)         => (d, insertElement2(acc, e, d))
    }._2
  }


  def addNs(
    model: Seq[Element],
    path0: Seq[(String, String)],
    refAttr: String,
    refNs0: String,
    attrName: String = ""
  )(implicit nsMap: Map[String, MetaNs]): Seq[Element] = {

    //    println(path0)
    //    println(refAttr)
    //    println(refNs0)
    //    println(attrName)

    val refNs = refNs0
    val ns0   = path0.last._2

    val refAttrClean = clean(refAttr)

    val bond = nsMap(ns0).attrs.collectFirst {
      case MetaAttr(_, `refAttr`, card, _, _, _, _, _, _, _, _, _, _) =>
        Bond(ns0, refAttr, refNs, card)
    }.get // In our closed eco-system we can expect the refAttr to be present

    val attr: GenericAtom = if (attrName.isEmpty)
      Atom(refNs, dummy, "", 1, NoValue, None, List(), List())
    else nsMap(refNs).attrs.collectFirst {
      case MetaAttr(_, `attrName`, _, "datom", _, _, _, _, _, _, _, _, _) =>
        Generic(refNs, attrName, "datom", EntValue)

      case MetaAttr(_, `attrName`, card, tpe1, Nil, _, _, _, _, _, _, _, _) =>
        Atom(refNs, attrName, tpe1, card, VarValue)

      case MetaAttr(_, `attrName`, card, tpe1, _, _, _, _, _, _, _, _, _) =>
        Atom(refNs, attrName, tpe1, card, EnumVal, Some(s":$refNs.$attrName/"))
    }.get

    def rebonds(path: Seq[(String, String)]): Seq[ReBond] = {
      val pathDiff = path.diff(path0)
      val pathAll  = path0.last +: pathDiff
      val path2    = pathAll.init.reverse
      val reBonds  = path2.map {
        case (_, ns) => ReBond(ns)
      }
      reBonds
    }

    val (before, branch0, after) = isolateBranch(model, path0)
    val last                     = branch0.size - 1

    val branch1 = branch0.zipWithIndex.foldLeft(
      0, Seq.empty[Element], Seq.empty[(String, String)]
    ) {
      case ((0, Nil, _), (Atom(_, `dummy`, _, _, _, _, _, _), `last`)) =>
        (1, Seq(bond, attr), path0)

      case ((0, Nil, _), (Atom(_, refAttr1, _, _, _, _, _, _), `last`))
        if clean(refAttr1) == refAttrClean =>
        val meta = Generic(refNs, "e", "datom", EntValue)
        attr match {
          case `meta` => (1, Seq(bond, attr), Nil)
          case _      => (1, Seq(bond, meta, attr), Nil)
        }

      case ((0, Nil, _), (e, `last`)) =>
        (1, Seq(e, bond, attr), path0)

      case ((0, acc, path), (e, `last`)) if path != path0 =>
        (1, (acc :+ e) ++ (rebonds(path) :+ bond :+ attr), Nil)

      case ((0, Nil, _), (a: GenericAtom, _)) =>
        (0, Seq(a), path0)

      case ((0, Nil, _), (b: Bond, _)) =>
        (0, Seq(b), path0 :+ b.refAttr -> b.refNs)

      case ((0, acc, path), (r: ReBond, _)) if path0 == path =>
        (1, (acc :+ bond :+ attr) ++ (rebonds(path :+ refAttr -> refNs) :+ r),
          path :+ refAttr -> refNs)
      case ((done, acc, path), (b: Bond, _))                 =>
        (done, acc :+ b, path :+ b.refAttr -> b.refNs)
      case ((done, acc, path), (r: ReBond, _))               =>
        (done, acc :+ r, path.init)

      case ((done, acc, path), (Atom(_, refAttr1, _, _, _, _, _, _), `last`))
        if clean(refAttr1) == refAttrClean =>
        if (attr.attr == "e") {
          (1, acc ++ (rebonds(path) :+ bond :+ attr), Nil)
        } else {
          val generic = Generic(refNs, "e", "datom", EntValue)
          (1, acc ++ (rebonds(path) :+ bond :+ generic :+ attr), Nil)
        }

      case ((done, acc, path), (lastAttr, `last`)) if acc.exists {
        case Atom(_, refAttr1, _, _, _, _, _, _)
          if clean(refAttr1) == refAttrClean => true
        case other                           => false
      } =>
        val previousWithoutRefAttr = acc.filterNot {
          case Atom(_, refAttr1, _, _, _, _, _, _)
            if clean(refAttr1) == refAttrClean => true
        }
        val generic                = Generic(refNs, "e", "datom", EntValue)
        val newElements            = rebonds(path) :+ bond :+ generic :+ attr
        (1, (previousWithoutRefAttr :+ lastAttr) ++ newElements, Nil)

      case ((done, acc, path), (lastAttr, `last`)) =>
        (1, (acc :+ lastAttr) ++ (rebonds(path) :+ bond :+ attr), Nil)

      case ((done, acc, path), (e, _)) =>
        (done, acc :+ e, path)
    }._2

    //    debugBeforeAfter(before, branch1, after)

    before ++ metaFirst(branch1) ++ after
  }

  private def metaFirst(branch: Seq[Element]): Seq[Element] =
    branch.foldLeft(0, Seq.empty[Element]) {
      case ((0, acc), meta@Generic(_, "e" | "e_", _, _)) => (0, meta +: acc)
      case ((0, acc), b: Bond)                           => (1, acc :+ b)
      case ((done, acc), e)                              => (done, acc :+ e)
    }._2

  def removeBranch(model: Seq[Element], path0: Seq[(String, String)]): Seq[Element] = {
    val (before, _, after) = isolateBranch(model, path0)
    val ns                 = path0.init.last._2
    val dummyAtom          = Atom(ns, dummy, "", 1, NoValue)
    val before1            = before match {
      case Seq(_: Bond) => Seq(dummyAtom)
      case _            => before.foldRight(0, Seq.empty[Element]) {
        case (a: GenericAtom, (0, acc))             => (1, a +: acc)
        case (Bond(`ns`, _, _, _, _), (0, acc))     => (1, acc)
        case (b@Bond(_, _, `ns`, _, _), (1, acc))   => (2, b +: dummyAtom +: acc)
        case (_: ReBond, (1, acc)) if after.isEmpty => (1, acc)
        case (r@ReBond(`ns`), (1, acc))             => (2, r +: acc)
        case (e, (1, acc))                          => (2, e +: acc)
        case (e, (2, acc))                          => (2, e +: acc)
      }._2
    }
    //    debugBeforeAfter(before, branch0, after)
    //    println(Model(before1 ++ after).code)
    before1 ++ after
  }

  def attrSelection(attrs0: Seq[MetaAttr],
                    selAttrs: Seq[GenericAtom],
                    refs0: Seq[(String, MetaNs)],
                    selection: String): (Seq[MetaAttr], Seq[(String, Int, MetaNs)]) = {
    val attrCardinalities = attrs0.map(a => a.name -> a.card).toMap
    selection match {
      case "m"   =>
        //        println("m")
        val selAttrNames = selAttrs.map(_.attr)
        val attrs        = attrs0.collect {
          case a@MetaAttr(_, attr, _, _, _, _, _, _, _, _, _, _, _)
            if selAttrNames.exists(_.startsWith(attr)) => a
        }
        (attrs, Nil)
      case "v"   =>
        //        println("v")
        val attrs     = attrs0.head +: attrs0.tail.filter(_.entityCount$.isDefined)
        val attrNames = attrs.map(_.name)
        val refs      = refs0.filter(r => attrNames.contains(r._1)).map {
          r =>
            val (refAttr, refNs) = r
            val refNsAttrs       = refNs.attrs
            val refAttrs         = refNsAttrs.head +:
              refNsAttrs.tail.filter(_.entityCount$.nonEmpty)
            val refNs1           = refNs.copy(attrs = refAttrs)
            (refAttr, attrCardinalities(refAttr), refNs1)
        }
        (attrs, refs)
      case "r"   =>
        //        println("r")
        val attrs     = attrs0.head +:
          attrs0.tail.filter(a => a.entityCount$.isDefined && a.tpe != "ref")
        val attrNames = attrs0.filter(a => a.entityCount$.isDefined).map(_.name)
        val refs      = refs0.filter(r => attrNames.contains(r._1)).map {
          r =>
            val (refAttr, refNs) = r
            val refNsAttrs       = refNs.attrs
            val refAttrs         = refNsAttrs.head +:
              refNsAttrs.tail.filter(a => a.entityCount$.nonEmpty && a.tpe != "ref")
            val refNs1           = refNs.copy(attrs = refAttrs)
            (refAttr, attrCardinalities(refAttr), refNs1)
        }
        (attrs, refs)
      case other =>
        //        println(other)
        (attrs0, refs0.map(r => (r._1, attrCardinalities(r._1), r._2)))
    }
  }

  def getAdditives(selAttrs: Seq[GenericAtom]): Map[String, Seq[String]] = {
    selAttrs.collect {
      case Atom(nsFull, attr, _, _, _, _, _, additives)
        if additives.nonEmpty && additives.head != "edit"
      => s"$nsFull-${clean(attr)}" -> additives
    }.toMap
  }


  def attrFullClass(selAttrs: Seq[GenericAtom],
                    selAttr: String,
                    refNs: Option[String]): (String, String, Value) = {
    val (attr1, cls, attrValue) = selAttrs.collectFirst {
      case Generic(_, `selAttr`, _, value) =>
        (selAttr, "attr-mandatory", value)

      case Atom(_, `selAttr`, _, _, value, _, _, _) =>
        (selAttr, "attr-mandatory", value)

      case Generic(_, "e_", _, value) if selAttr == "e" =>
        ("e_", "attr-tacit", value)

      case Atom(_, attr, _, _, Fn("not", None), _, _, _)
        if attr.last == '_' && attr.init == selAttr =>
        (attr, "attr-nil", Fn("not", None))

      case Atom(_, attr, _, _, value, _, _, _)
        if attr.last == '_' && attr.init == selAttr =>
        (attr, "attr-tacit", value)

      case Atom(_, attr, _, _, value, _, _, _)
        if attr.last == '$' && attr.init == selAttr =>
        (attr, "attr-optional", value)

    }.getOrElse((selAttr, "attr-none", VarValue))

    //    selAttrs foreach println
    //    println(s"attrFullClass: $attr1  -  $cls  -  $attrValue")

    (cls, refNs) match {
      case ("attr-none", Some(_)) => (attr1, "attr-ref", attrValue)
      case _                      => (attr1, cls, attrValue)
    }
  }


  def modelTreeFormatted(tree: Seq[Any], depth: Int = 0): String = {
    val indent = "   " * depth
    val body   = tree.map {
      case branch: Seq[_]     => modelTreeFormatted(branch, depth + 1)
      case attribute: Element => indent + "   " + attribute
      case other              => indent + "   " + other
    }.mkString(",\n")
    s"${indent}List(\n" + body + s"\n$indent)"
  }
}
