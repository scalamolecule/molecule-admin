package moleculeadmin.server

import db.admin.dsl.moleculeAdmin._
import molecule.api.out20._
import molecule.facade.Conn
import moleculeadmin.server.schema._
import moleculeadmin.server.utils.DefFile
import moleculeadmin.shared.api.SchemaApi
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.ops.query.SchemaOps

class Schema extends SchemaApi with SchemaBase with Base with SchemaOps {

  override def getLiveSchema(db: String): FlatSchema = {
    implicit val conn = Conn(base + "/" + db)
    Schema.a.part.ns.nsFull.attr.card.tpe.doc$.fulltext$.isComponent$.unique$.noHistory$.get.sortBy(r => r._1).collect {
      case (_, part, ns, nsFull, attr, card0, tpe0, doc$, fulltext$, isComponent$, unique$, noHistory$) if
      (part.nonEmpty && !part.startsWith("-")) ||
        (part.isEmpty && !ns.startsWith("-"))
      =>
        val tpe                  = tpeDatomicMolecule(tpe0)
        val card                 = if (card0 == "one") 1 else 2
        val options: Seq[String] = Seq(
          if (fulltext$.isDefined) Some("fulltext") else None,
          if (isComponent$.isDefined) Some("isComponent") else None,
          if (noHistory$.isDefined) Some("noHistory") else None,
          unique$,
        ).flatten
        FlatAttr(0, part, None, ns, nsFull, None, attr, card, tpe, options = options, doc$ = doc$)
    }
  }

  override def updateSchemaCounts(db: String): Unit = Values.updateSchemaCounts(db)

  override def updateDescrAttr(db: String, part: String, ns: String, attr: String, descrAttr: Option[String]): FlatSchema = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    println(s"updateDescrAttr: ${getFullAttr(part, ns, attr)}   $descrAttr")
    val attrId: Long = meta_Db.name_(db).Partitions.name_(part).Namespaces.name_(ns).Attrs.e.name_(attr).get.head

    // Set describing attribute and retract topValues
    meta_Attribute(attrId).descrAttr$(descrAttr).topValues().update

    // get flat meta schema
    meta_Db.name_(db)
      .Partitions.pos.name.descr$
      .Namespaces.pos.name.nameFull.descr$
      .Attrs.pos.name.card.tpe.enums$.refNs$.options$.doc$
      .attrGroup$.entityCount$.distinctValueCount$.descrAttr$.TopValues.*?(
      stats_TopValue.entityCount.value.label$
    ).get.sortBy(t => (t._1, t._4, t._8)).zipWithIndex.map {
      case ((_, part, partDescr, _, ns, nsFull, nsDescr, _, attr, card, tpe,
      enums0, ref, options0, doc,
      attrGroup, count, distinctCount, descrAttr, topValuesList), i) => {
        val topValues1: Seq[TopValue] = topValuesList.map(TopValue tupled)
        val topValues2: Seq[TopValue] = if (topValues1.isEmpty)
          Nil
        else if (topValues1.head.label$.isDefined)
          topValues1.sortBy(r => (r.entityCount, r.label$.getOrElse("")))
        else
          topValues1.sortBy(r => (r.entityCount, r.value))

        FlatAttr(
          i + 1, part, partDescr, ns, nsFull, nsDescr, attr, card, tpe,
          enums0.getOrElse(Nil).toSeq.sorted,
          ref,
          options0.getOrElse(Nil).toSeq.filterNot(_ == "indexed").sorted,
          doc, attrGroup, count, distinctCount, descrAttr, topValues2
        )
      }
    }
  }

  override def getFlatSchemas(db: String): (FlatSchema, FlatSchema, FlatSchema) = {
    val liveSchema: FlatSchema = getLiveSchema(db)
    val defSchema : FlatSchema = DefFile(db).getDefFileSchema
    val metaSchema: MetaSchema = getMetaSchema(db)
    (liveSchema, defSchema, mkFlatAttrs(metaSchema))
  }

  override def getSchemas(db: String): (FlatSchema, FlatSchema, FlatSchema, MetaSchema) = {
    val liveSchema: FlatSchema = getLiveSchema(db)
    val defSchema : FlatSchema = DefFile(db).getDefFileSchema
    val metaSchema: MetaSchema = getMetaSchema(db)
    (liveSchema, defSchema, mkFlatAttrs(metaSchema), metaSchema)
  }

  override def getSchemas2(db: String): (Seq[String], FlatSchema, MetaSchema) = {
    val metaSchema: MetaSchema = getMetaSchema(db)
    (dbNames, mkFlatAttrs(metaSchema), metaSchema)
  }

  override def createPartition(
    schema: MetaSchema,
    db: String,
    part: String,
    descr: Option[String],
    pos0: Int
  ): Either[String, MetaSchema] =
    Partition.create(schema, db, part, descr, pos0)

  override def updatePartition(
    schema0: MetaSchema,
    db: String,
    curPart: String,
    newPart: String,
    descr: Option[String],
    pos0: Int
  ): Either[String, MetaSchema] =
    Partition.update(schema0, db, curPart, newPart, descr, pos0)

  override def deletePartition(
    schema: MetaSchema,
    db: String,
    part: String
  ): Either[String, MetaSchema] =
    Partition.delete(schema, db, part)


  override def createNamespace(
    schema: MetaSchema,
    db: String,
    part: String,
    ns: String,
    descr: Option[String],
    pos0: Int
  ): Either[String, MetaSchema] =
    Namespace.create(schema, db, part, ns, descr, pos0)

  override def updateNamespace(
    schema0: MetaSchema,
    db: String,
    part: String,
    curtNs: String,
    newNs: String,
    descr: Option[String],
    pos0: Int
  ): Either[String, MetaSchema] =
    Namespace.update(schema0, db, part, curtNs, newNs, descr, pos0)

  override def deleteNamespace(
    schema: MetaSchema,
    db: String,
    part: String,
    ns: String
  ): Either[String, MetaSchema] =
    Namespace.delete(schema, db, part, ns)


  override def createAttribute(
    schema: MetaSchema,
    db: String,
    part: String,
    ns: String,
    attr: String,
    card: Int,
    tpe: String,
    enums: Seq[String],
    optRefNs: Option[String],
    options: Seq[String],
    doc: Option[String],
    pos0: Int
  ): Either[String, MetaSchema] =
    Attribute.create(schema, db, part, ns, attr, card, tpe, enums, optRefNs, options, doc, pos0)

  override def updateAttribute(
    schema: MetaSchema,
    db: String,
    part: String,
    ns: String,
    curAttr: String,
    newAttr: String,
    card: Int,
    tpe: String,
    enums: Seq[String],
    optRefNs: Option[String],
    options: Seq[String],
    doc: Option[String],
    pos0: Int,
    attrGroup: Option[String]
  ): Either[String, MetaSchema] =
    Attribute.update(schema, db, part, ns, curAttr, newAttr, card, tpe, enums, optRefNs, options, doc, pos0, attrGroup)

  override def deleteAttribute(
    schema: MetaSchema,
    db: String,
    part: String,
    ns: String,
    attr: String
  ): Either[String, MetaSchema] =
    Attribute.delete(schema, db, part, ns, attr)


  // Non-boot partitions
  def getPartitions(conn: Conn): List[String] = conn.q(
    """[:find ?part
      | :where [_ :db.install/partition ?partId]
      |        [(> ?partId 10)]
      |        [?partId :db/ident ?partIdent]
      |        [(name ?partIdent) ?part]
      |        ]
    """.stripMargin).flatten.map(_.toString).sorted
}