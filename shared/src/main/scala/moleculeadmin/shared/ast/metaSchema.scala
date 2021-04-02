package moleculeadmin.shared.ast
import moleculeadmin.shared.util.HelpersAdmin

object metaSchema extends HelpersAdmin {

  case class TopValue(entityCount: Int, value: String, label$: Option[String]) {
    override def toString = s"""TopValue($entityCount, "$value", ${o(label$)})"""
  }


  type FlatSchema = Seq[FlatAttr]

  case class FlatAttr(
    pos: Int,
    part: String,
    partDescr$: Option[String],
    ns: String,
    nsFull: String,
    nsDescr$: Option[String],
    attr: String,
    card: Int,
    tpe: String,
    enums: Seq[String] = Nil,
    refNs$: Option[String] = None,
    options: Seq[String] = Nil,
    doc$: Option[String] = None,
    attrGroup$: Option[String] = None,
    entityCount$: Option[Int] = None,
    distinctValueCount$: Option[Int] = None,
    descrAttr$: Option[String] = None,
    topValues: Seq[TopValue] = Nil
  ) {
    override def toString: String =
      s"""FlatAttr($pos, "$part", ${o(partDescr$)}, "$ns", "$nsFull", ${o(nsDescr$)}, "$attr", $card, "$tpe", """ +
        s"""${seq(enums)}, ${o(refNs$)}, ${seq(options)}, ${o(doc$)}, """ +
        s"""${o(attrGroup$)}, ${o(entityCount$)}, ${o(distinctValueCount$)}, ${o(descrAttr$)}, Seq(${
          if (topValues.isEmpty) "" else topValues.mkString("\n        ", ",\n        ", "")
        }))"""

  }


  case class MetaSchema(
    parts: Seq[MetaPart]
  ) {
    override def toString =
      s"""MetaSchema(Seq(${
        if (parts.isEmpty) "" else parts.mkString("\n  ", ",\n  ", "")
      }))"""
  }


  case class MetaPart(
    pos: Int,
    name: String,
    descr$: Option[String] = None,
    entityCount$: Option[Int] = None,
    nss: Seq[MetaNs] = Nil
  ) {
    override def toString =
      s"""MetaPart($pos, "$name", ${o(descr$)}, ${o(entityCount$)}, Seq(${
        if (nss.isEmpty) "" else nss.mkString("\n    ", ",\n    ", "")
      }))"""
  }


  case class MetaNs(
    pos: Int,
    name: String,
    nameFull: String,
    descr$: Option[String] = None,
    entityCount$: Option[Int] = None,
    attrs: Seq[MetaAttr] = Nil
  ) {
    override def toString =
      s"""MetaNs($pos, "$name", "$nameFull", ${
        descr$.fold("None")(t => "Some(\"\"\"" + t + "\"\"\")")
      }, ${o(entityCount$)}, Seq(${
        if (attrs.isEmpty) "" else attrs.mkString("\n      ", ",\n      ", "")
      }))"""
  }


  case class MetaAttr(
    pos: Int,
    name: String,
    card: Int,
    tpe: String,
    enums: Seq[String] = Nil,
    refNs$: Option[String] = None,
    options: Seq[String] = Nil,
    doc$: Option[String] = None,
    attrGroup$: Option[String] = None,
    entityCount$: Option[Int] = None,
    distinctValueCount$: Option[Int] = None,
    descrAttr$: Option[String] = None,
    topValues: Seq[TopValue] = Nil
  ) {
    override def toString: String =
      s"""MetaAttr($pos, "$name", $card, "$tpe", ${seq(enums)}, ${o(refNs$)}, ${seq(options)}, ${o(doc$)}, """ +
        s"""${o(attrGroup$)}, ${o(entityCount$)}, ${o(distinctValueCount$)}, ${o(descrAttr$)}, Seq(${
          if (topValues.isEmpty) "" else topValues.mkString("\n        ", ",\n        ", "")
        }))"""
  }

}