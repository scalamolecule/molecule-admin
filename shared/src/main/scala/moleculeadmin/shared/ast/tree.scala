package moleculeadmin.shared.ast
import molecule.ast.model.GenericAtom
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.util.HelpersAdmin

object tree extends HelpersAdmin {

  case class Tree(path: Seq[(String, String)],
                  attrs: Seq[Attr],
                  selAttrs: Seq[GenericAtom],
                  refs: Seq[(String, Ns)],
                  selRefs: Seq[String],
                  branches: Seq[Tree]) {

    override def toString = {

      def recurse(branches: Seq[Tree], indent: Int): String = {
        val s = "   " * indent
        branches.map { case Tree(path1, attrs1, selAttrs1, refs1, selRefs1, branches1) =>
          s"""|
              |${s}Tree(List(${path1.map { case (refAttr, refNs) => s""""$refAttr" -> "$refNs"""" }.mkString(s", ")}),
              |$s     List("${attrs1.grouped(7).map(_.map(_.name).mkString("\", \"")).mkString(s",\n$s          ")}"),
              |$s     List(${selAttrs1.mkString(s",\n$s          ")}),
              |$s     List(${refs1.grouped(3).map(_.map(r => s""""${r._1}" -> "${r._2.nameFull}"""").mkString(", ")).mkString(s",\n$s          ")}),
              |$s     List(${selRefs1.map(r => s""""$r"""").mkString(", ")}),
              |$s     List(${recurse(branches1, indent + 2)}))""".stripMargin
        }.mkString(s",")
      }
      if (path.isEmpty) {
        println(branches)
        ""
      } else {
        s"""Tree(List("${path.head._1}" -> "${path.head._2}"),
           |     List("${attrs.grouped(7).map(_.map(_.name).mkString("\", \"")).mkString(",\n          ")}"),
           |     List(${selAttrs.mkString(",\n          ")}),
           |     List(${refs.grouped(3).map(_.map(r => s""""${r._1}" -> "${r._2.nameFull}"""").mkString(", ")).mkString(",\n          ")}),
           |     List(${selRefs.map(r => s""""$r"""").mkString(", ")}),
           |     List(${recurse(branches, 2)}))""".stripMargin
      }
    }

    def code = {
      def o(v: Option[Any]): String = v match {
        case None                             => "None"
        case Some(set: Set[_]) if set.isEmpty => s"""Some(Set())"""
        case Some(set: Set[_])                => s"""Some(Set(${set.map(v => s""""$v"""").mkString(", ")}))"""
        case Some(other)                      => s"""Some("${other.toString}")"""
      }
      def attr(a: Attr) = a match {
        case Attr(order, name, card, tpe, enums, ref, options, doc, attrGroup, attrValueCount, attrDistinctValueCount, descrAttr, topValues) =>
          s"""($order, "$name", $card, "$tpe", ${o(enums)}, ${o(ref)}, ${o(options)}, ${o(doc)}, ${o(attrGroup)}, ${o(attrValueCount)}, ${o(attrDistinctValueCount)}, ${o(descrAttr)}, ${topValues.take(2).mkString(", ") + ", ..."})"""
      }

      def recurse(branches: Seq[Tree], indent: Int): String = {
        val s = "   " * indent
        branches.map { case Tree(path1, attrs1, selAttrs1, refs1, selRefs1, branches1) =>
          s"""|
              |${s}Tree(List(${path1.map { case (refAttr, refNs) => s""""$refAttr" -> "$refNs"""" }.mkString(s", ")}),
              |$s     List(${attrs1.map(attr).mkString(s",\n$s          ")}),
              |$s     List(${selAttrs1.mkString(s",\n$s          ")}),
              |$s     List(${refs1.grouped(3).map(_.map(r => s""""${r._1}" -> "${r._2.nameFull}"""").mkString(", ")).mkString(s",\n$s          ")}),
              |$s     List(${selRefs1.map(r => s""""$r"""").mkString(", ")}),
              |$s     List(${recurse(branches1, indent + 2)}))""".stripMargin
        }.mkString(s",")
      }

      s"""Tree(List("${path.head._1}" -> "${path.head._2}"),
         |     List(${attrs.map(attr).mkString(s",\n          ")}),
         |     List(${selAttrs.mkString(",\n          ")}),
         |     List(${refs.grouped(3).map(_.map(r => s""""${r._1}" -> "${r._2.nameFull}"""").mkString(", ")).mkString(",\n          ")}),
         |     List(${selRefs.map(r => s""""$r"""").mkString(", ")}),
         |     List(${recurse(branches, 2)}))
       """.stripMargin
    }

    def code2 = {
      def o(v: Option[Any]): String = v match {
        case None                             => "None"
        case Some(set: Set[_]) if set.isEmpty => s"""Some(Set())"""
        case Some(set: Set[_])                => s"""Some(Set(${set.map(v => s""""$v"""").mkString(", ")}))"""
        case Some(other)                      => s"""Some("${other.toString}")"""
      }
      def attr(a: Attr) = a match {
        case Attr(order, name, card, tpe, enums, ref, options, doc, attrGroup, attrValueCount, attrDistinctValueCount, descrAttr, topValues) =>
          s"""($order, "$name", $card, "$tpe", ${o(enums)}, ${o(ref)}, ${o(options)}, ${o(doc)}, ${o(attrGroup)}, ${o(attrValueCount)}, ${o(attrDistinctValueCount)}, ${o(descrAttr)}, ${topValues.take(2).mkString(", ") + ", ..."})"""
      }
      def ns(n: Ns, s: String) = n match {
        case Ns(order, name, nameFull, nsDescr, nsCount, attrs) => s"""($order, "$name", "$nameFull", "$nsDescr", List(${attrs.map(attr).mkString(s"\n$s            ", s",\n$s            ", ")")})"""
      }

      def recurse(branches: Seq[Tree], indent: Int): String = {
        val s = "   " * indent
        branches.map { case Tree(path1, attrs1, selAttrs1, refs1, selRefs1, branches1) =>
          s"""|
              |${s}Tree(List(${path1.map { case (refAttr, refNs) => s""""$refAttr" -> "$refNs"""" }.mkString(s", ")}),
              |$s     List(
              |$s       ${attrs1.map(attr).mkString(s",\n$s          ")}),
              |$s     List(${selAttrs1.mkString(s",\n$s          ")}),
              |$s     List(
              |$s       ${refs1.map(r => s""""${r._1}" -> ${ns(r._2, s)}""").mkString(s",\n$s          ")}),
              |$s     List(${selRefs1.map(r => s""""$r"""").mkString(", ")}),
              |$s     List(${recurse(branches1, indent + 2)}))""".stripMargin
        }.mkString(s",")
      }

      s"""Tree(List("${path.head._1}" -> "${path.head._2}"),
         |     List(
         |       ${attrs.map(attr).mkString(s",\n          ")}),
         |     List(${selAttrs.mkString(",\n          ")}),
         |     List(
         |       ${refs.map(r => s""""${r._1}" -> ${ns(r._2, "")}""").mkString(",\n          ")}),
         |     List(${selRefs.map(r => s""""$r"""").mkString(", ")}),
         |     List(${recurse(branches, 2)}))
       """.stripMargin
    }
  }
}