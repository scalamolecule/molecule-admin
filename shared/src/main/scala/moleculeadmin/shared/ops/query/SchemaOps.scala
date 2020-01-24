package moleculeadmin.shared.ops.query

import moleculeadmin.shared.api.QueryApi
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ast.schema._


trait SchemaOps extends QueryApi with BaseQuery {

  def getFilteredSchema(schema: MetaSchema, selection: String): MetaSchema = selection match {
    case "v" => MetaSchema(
      for (Part(partN, part, partDescr, entityCount, nss) <- schema.parts if entityCount.isDefined && entityCount.get != 0) yield {
        val nss2 = for {
          Ns(nsN, ns, nsFull, nsDescr, entityCount, attrs) <- nss if entityCount.isDefined && entityCount.get != 0
        } yield {
          val attrs2 = for {
            attr <- attrs if attr.entityCount$.isDefined && attr.entityCount$.get != 0
          } yield attr
          Ns(nsN, ns, nsFull, nsDescr, entityCount, attrs2)
        }
        Part(partN, part, partDescr, entityCount, nss2)
      }
    )

    case "r" => MetaSchema(
      for (Part(partN, part, partDescr, entityCount, nss) <- schema.parts if entityCount.isDefined && entityCount.get != 0) yield {
        val nss2 = for {
          Ns(nsN, ns, nsFull, nsDescr, entityCount, attrs) <- nss if entityCount.isDefined && entityCount.get != 0
        } yield {
          val attrs2 = for {
            attr <- attrs if attr.entityCount$.isDefined && attr.entityCount$.get != 0 && attr.tpe != "ref"
          } yield attr
          Ns(nsN, ns, nsFull, nsDescr, entityCount, attrs2)
        }
        Part(partN, part, partDescr, entityCount, nss2)
      }
    )
    case _   => schema
  }


  def mkNsMap(metaSchema: MetaSchema): Map[String, Ns] = {
    val initialEntityAttr = Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil)
    val nsMap             = (for {
      Part(_, _, _, _, nss) <- metaSchema.parts
      Ns(i, ns, nsFull, nsDescr, nsCount, attrs) <- nss
    } yield nsFull -> Ns(i, ns, nsFull, nsDescr, nsCount, initialEntityAttr +: attrs)).toMap

    //    nsMap.map { case (ns, nsDef) => s""""$ns" -> $nsDef,""" } foreach println
    nsMap
  }


  def mkViewCellTypes(nsMap: Map[String, Ns]): Map[String, String] = {
    val attrs: Map[String, String] = for {
      (nsFull, nsDef) <- nsMap
      attr <- nsDef.attrs
    } yield {
      val cellType = (attr.tpe match {
        case "String" if attr.enums$.isDefined                             => "enum"
        case "String"                                                      => "str"
        case "Int" | "Long" | "Float" | "Double" | "BigInt" | "BigDecimal" => "num"
        case "ref"                                                         => "ref"
        case "Date"                                                        => "date"
        // Boolean, UUID, URI,
        case _ => "other"
      }) + (attr.card match {
        case 2 => "Set"
        case 3 => "Map"
        case _ => ""
      })
      s":$nsFull/${attr.name}" -> cellType
    }
    // add generic db attributes
    attrs ++ Map(
      ":db/id" -> "otherStr",
      ":db/txInstant" -> "otherStr"
    )
  }


  def mkEnumAttrs(nsMap: Map[String, Ns]): Seq[String] = (for {
    (nsFull, nsDef) <- nsMap
    attr <- nsDef.attrs if attr.enums$.isDefined
  } yield {
    s":$nsFull/${attr.name}"
  }).toSeq


  def mkFlatAttrs(metaSchema: MetaSchema): FlatSchema = {
    var n = 0
    for {
      Part(_, part, partDescr, _, nss) <- metaSchema.parts.sortBy(_.pos)
      Ns(_, ns, nsFull, nsDescr, _, attrs) <- nss.sortBy(_.pos)
      Attr(_, attr, card, tpe, enums, refNs, options, doc, aGr, count, valCount, descrAttr, topValues) <- attrs.sortBy(_.pos)
    } yield {
      n += 1
      FlatAttr(n, part, partDescr, ns, nsFull, nsDescr, attr, card, tpe, enums.getOrElse(Nil).toSeq,
        refNs, options.getOrElse(Nil).toSeq, doc, aGr, count, valCount, descrAttr, topValues)
    }
  }


}
