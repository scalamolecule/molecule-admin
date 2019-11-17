package moleculeadmin.server
import db.admin.dsl.meta._
import molecule.api.Entity
import molecule.api.out20._
import molecule.facade.Conn
import moleculeadmin.shared.api.BaseApi
import moleculeadmin.shared.ast.query.{ColSetting, SavedQuery}
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.util.HelpersAdmin

trait Base extends BaseApi with HelpersAdmin {


  override def dbNames(): Seq[String] = {
    implicit val conn = Conn(base + "/meta")
    meta_Db.name.isMolecular_(true).get.sorted
  }

  def settings(db: String): (Set[String], Seq[SavedQuery]) = {
    implicit val conn = Conn(base + "/meta")
    // Use "admin" for now. Todo: users
    val userId                 = user_User.e.username_("admin").get match {
      case List(eid) => eid
      case Nil       => user_User.username("admin").save.eid
    }
    val dbId                   = meta_Db.e.name_(db).get.head
    val openViews: Set[String] =
      user_User(userId).views.get.headOption.getOrElse(Set.empty[String])
    val queries                = user_User(userId)
      .DbSettings.db_(dbId)
      .Queries.molecule
      .ColSettings.*(user_ColSetting.index.attrExpr.sortDir.sortPos)
      .get.sortBy(_._1)
      .map {
        case (molecule, colSettings) =>
          SavedQuery(molecule, colSettings.map(ColSetting.tupled(_)))
      }
    (openViews, queries)
  }

  override def loadMetaData(db: String): (Seq[String], MetaSchema, (Set[String], Seq[SavedQuery])) =
    (dbNames(), getMetaSchema(db), settings(db))

  override def getMetaSchema(db: String): MetaSchema = {
    implicit val conn = Conn(base + "/meta")

    val dbE: Long = {
      val dbEntitites = meta_Db.e.name_(db).get
      if (dbEntitites.isEmpty)
        throw new RuntimeException(s"Couldn't find database `$db` in meta_Db. " +
          s"Has its definition file path been saved (check on `Dbs` page)?")
      dbEntitites.head
    }
    val schemaRaw = Entity(conn.db.entity(dbE), conn, dbE.asInstanceOf[Object]).touch

    def cleanOptions(options: Option[List[String]]): Option[Set[String]] = options match {
      case None                         => Option.empty[Set[String]]
      case Some(opts) if opts.size == 1 => Option.empty[Set[String]]
      case Some(opts)                   =>
        Some(opts
          .map(_.substring(24))
          .filterNot(_ == "indexed").toSet) // remove :meta_Attribute.options/
    }

    val parts = for {
      parts <- schemaRaw.get(":meta_Db/partitions").asInstanceOf[Option[List[Map[String, Any]]]].toSeq
      part <- parts
    } yield {
      Part(
        part(":meta_Partition/pos").asInstanceOf[Long].toInt,
        part(":meta_Partition/name").asInstanceOf[String],
        part.get(":meta_Partition/descr").asInstanceOf[Option[String]],
        part.get(":meta_Partition/entityCount").asInstanceOf[Option[Long]].map(_.toInt),
        (for {
          nss <- part.get(":meta_Partition/namespaces").asInstanceOf[Option[List[Map[String, Any]]]].toSeq
          ns <- nss
        } yield {
          Ns(
            ns(":meta_Namespace/pos").asInstanceOf[Long].toInt,
            ns(":meta_Namespace/name").asInstanceOf[String],
            ns(":meta_Namespace/nameFull").asInstanceOf[String],
            ns.get(":meta_Namespace/descr").asInstanceOf[Option[String]],
            ns.get(":meta_Namespace/entityCount").asInstanceOf[Option[Long]].map(_.toInt),
            (for {
              attrs <- ns.get(":meta_Namespace/attrs").asInstanceOf[Option[List[Map[String, Any]]]].toSeq
              attr <- attrs
            } yield {
              val attrType                 = attr(":meta_Attribute/tpe").asInstanceOf[String].substring(20) // remove :meta_Attribute.tpe/
              val maybeTopValues           = attr.get(":meta_Attribute/topValues").asInstanceOf[Option[List[Map[String, Any]]]]
              val card                     = attr(":meta_Attribute/card").asInstanceOf[Long].toInt
              val topValues: Seq[TopValue] = if (maybeTopValues.isEmpty) Nil else {
                val topValues1: Seq[TopValue] = for {
                  topValues <- maybeTopValues.toSeq
                  topValue <- topValues
                } yield {
                  TopValue(
                    topValue(":stats_TopValue/entityCount").asInstanceOf[Long].toInt,
                    topValue(":stats_TopValue/value").asInstanceOf[String],
                    topValue.get(":stats_TopValue/label").asInstanceOf[Option[String]]
                  )
                }
                val topValues2                = if (topValues1.head.label$.isDefined) {
                  topValues1.sortBy(v => (-v.entityCount, v.label$.getOrElse("")))
                } else {
                  attrType match {
                    case _ if card == 3 => topValues1.sortBy(v => (-v.entityCount, v.value))
                    case "Int"          => topValues1.sortBy(v => (-v.entityCount, v.value.toInt))
                    case "Long"         => topValues1.sortBy(v => (-v.entityCount, v.value.toLong))
                    case "Float"        => topValues1.sortBy(v => (-v.entityCount, v.value.toFloat))
                    case "Double"       => topValues1.sortBy(v => (-v.entityCount, v.value.toDouble))
                    case "BigInt"       => topValues1.sortBy(v => (-v.entityCount, BigInt(v.value)))
                    case "BigDecimal"   => topValues1.sortBy(v => (-v.entityCount, BigDecimal(v.value)))
                    case _              => topValues1.sortBy(v => (-v.entityCount, v.value))
                  }
                }
                topValues2
              }

              Attr(
                attr(":meta_Attribute/pos").asInstanceOf[Long].toInt,
                attr(":meta_Attribute/name").asInstanceOf[String],
                card,
                attrType,
                attr.get(":meta_Attribute/enums").asInstanceOf[Option[Seq[String]]].map(_.toSet),
                attr.get(":meta_Attribute/refNs").asInstanceOf[Option[String]],
                cleanOptions(attr.get(":meta_Attribute/options").asInstanceOf[Option[List[String]]]),
                attr.get(":meta_Attribute/doc").asInstanceOf[Option[String]],
                attr.get(":meta_Attribute/attrGroup").asInstanceOf[Option[String]],
                attr.get(":meta_Attribute/entityCount").asInstanceOf[Option[Long]].map(_.toInt),
                attr.get(":meta_Attribute/distinctValueCount").asInstanceOf[Option[Long]].map(_.toInt),
                attr.get(":meta_Attribute/descrAttr").asInstanceOf[Option[String]],
                topValues
              )
            }).sortBy(_.pos) // Seq[Attr]
          )
        }).sortBy(_.pos) // Seq[Ns]
      )
    }

    MetaSchema(parts.sortBy(_.pos))
  }

  override def getFlatMetaSchema(db: String): FlatSchema = {
    implicit val conn = Conn(base + "/meta")
    meta_Db.name_(db)
      .Partitions.pos.name.descr$
      .Namespaces.pos.name.nameFull.descr$
      .Attrs.pos.name.card.tpe.enums$.refNs$.options$.doc$.attrGroup$.entityCount$.distinctValueCount$.descrAttr$.topValues$
      .get.sortBy(t => (t._1, t._4, t._8)).zipWithIndex.map {
      case ((_, part, partDescr, _, ns, nsFull, nsDescr, _, attr, card, tpe, enums0, ref, options0, doc, attrGroup, count, distinctCount, descrAttr, topValues0), i) => {
        val enums                     = enums0.getOrElse(Nil).toSeq.sorted
        val options                   = options0.getOrElse(Nil).toSeq.filterNot(_ == "indexed").sorted
        val topValues1: Seq[TopValue] = if (topValues0.isEmpty) Nil else {
          val topValueIds = topValues0.getOrElse(Nil)
          stats_TopValue(topValueIds).entityCount.value.label$.get.map(TopValue tupled)
        }
        val topValues2                = if (topValues1.isEmpty)
          Nil
        else if (topValues1.head.label$.isDefined)
          topValues1.sortBy(r => (r.entityCount, r.label$.getOrElse("")))
        else
          topValues1.sortBy(r => (r.entityCount, r.value))

        FlatAttr(i + 1, part, partDescr, ns, nsFull, nsDescr, attr, card, tpe, enums, ref, options, doc, attrGroup, count, distinctCount, descrAttr, topValues2)
      }
    }
  }
}
