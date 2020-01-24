package moleculeadmin.server.schema

import java.util
import java.util.{List => jList, Map => jMap}
import datomic.{Peer, Util}
import db.admin.dsl.moleculeAdmin._
import molecule.api.out15._
import molecule.facade.Conn
import moleculeadmin.server.Base
import moleculeadmin.shared.ast.schema._
import moleculeadmin.server.utils.DefFile


object Partition extends SchemaBase  with Base{


  def create(schema: MetaSchema, db: String, part: String, descr: Option[String], pos0: Int): Either[String, MetaSchema] = {
    if (part.isEmpty) {
      Left("Empty partition name.")
    } else if (reservedPartitionNames.contains(part)) {
      Left("Partition can't have reserved name `tx` or `db`.")
    } else if (part.head.isUpper) {
      Left("Partition name should start with lowercase letter and match `[a-z][a-zA-Z0-9]*`.")
    } else if (!part.matches("[a-z][a-zA-Z0-9]*")) {
      Left("Partition name should match `[a-z][a-zA-Z0-9]*`.")
    } else {
      implicit val moleculeAdminConn = Conn(base + "/MoleculeAdmin")
      withTransactor {
        meta_Db.e.name_(db).get match {
          case Seq(dbE) => {
            val liveConn            = Conn(base + "/" + db)
            val curPartitions       = meta_Db.name_(db).Partitions.e.pos.name.get
            val pos                 = if (pos0 == 0) curPartitions.length + 1 else pos0
            var newPartitionE: Long = 0L

            if (curPartitions.exists(_._3 == part)) {
              Left(s"Partition `$part` already exists in database `$db`. Please choose another partition name.")
            } else try {

              // Add partition to live schema
              liveConn.transact(Util.list(
                Util.map(
                  ":db/ident", s":$part",
                  ":db/id", Peer.tempid(":db.part/db"),
                  ":db.install/_partition", ":db.part/db"
                )
              ).asInstanceOf[util.List[AnyRef]])

              // Add partition to meta db
              newPartitionE = meta_Partition.pos(pos).name(part).descr$(descr).save.eid
              meta_Db(dbE).partitions.assert(newPartitionE).update

              // Increase pos index for partitions after new partition
              curPartitions.filter(_._2 >= pos).foreach { case (e, n, _) =>
                val incrPos = n + 1
                meta_Partition(e).pos(incrPos).update
              }

              // Add partition to client schema
              val newPartition : Part       = Part(pos, part, descr, None, Nil)
              val updatedSchema: MetaSchema = MetaSchema(
                if (schema.parts.isEmpty) {
                  Seq(newPartition)
                } else {
                  schema.parts.flatMap {
                    case p if p.pos == 1 && pos == 1 => Seq(newPartition, p.copy(pos = p.pos + 1))
                    case p if p.pos == pos - 1       => Seq(p, newPartition)
                    case p if p.pos < pos - 1        => Seq(p)
                    case p                           => Seq(p.copy(pos = p.pos + 1))
                  }
                }
              )

              DefFile(db).recreateFrom(updatedSchema)

            } catch {
              case error: Throwable => try {
                // Restore partition order
                curPartitions.filter(_._2 >= pos).foreach { case (e, n, _) => meta_Partition(e).pos(n).update }

                // Delete obsolete new meta partition entity (for some reason we seem allowed to retract partition entities)
                if (newPartitionE > 0L) newPartitionE.retract

                // Restore def file
                DefFile(db).recreateFrom(schema)

                Left("Successfully rolled back from error: " + error.getMessage)
              } catch {
                case rollbackError: Throwable => Left("Unsuccessfully tried to roll back from error! " +
                  "Please check meta db that might be in an invalid state. Unexpected rollback error: " + rollbackError.getMessage)
              }
            }
          }

          case Nil           => Left(s"Couldn't find database `$db`.")
          case databaseCount => Left(s"Unexpectedly found $databaseCount databases named `$db`.")
        }
      }
    }
  }


  def update(schema0: MetaSchema, db: String, curPart: String, newPart: String, descr: Option[String], pos0: Int): Either[String, MetaSchema] = {
    if (newPart.isEmpty) {
      Left("Empty partition name.")
    } else if (reservedPartitionNames.contains(newPart)) {
      Left("Partition can't have reserved name `tx` or `db`.")
    } else if (newPart.head.isUpper) {
      Left("Partition name should start with lowercase letter and match `[a-z][a-zA-Z0-9]*`.")
    } else if (!newPart.matches("[a-z][a-zA-Z0-9]*")) {
      Left("Partition name should match `[a-z][a-zA-Z0-9]*`.")
    } else {
      implicit val moleculeAdminConn = Conn(base + "/MoleculeAdmin")
      withTransactor {
        meta_Db.e.name_(db).get.size match {
          case 1 => meta_Db.name_(db).Partitions.e.pos.name_(curPart).descr$.namespaces$.get match {

            // Current partition data
            case Seq((partE, curPos, curDescrOpt, nss)) => {
              val liveConn                             = Conn(base + "/" + db)
              val curPartitions                        = meta_Db.name_(db).Partitions.e.pos.name.get
              val pos                                  = if (pos0 == 0) curPos else pos0
              val partitionEntities: Map[String, Long] = curPartitions.map(a => a._3 -> a._1).toMap

              val rollbackRefNss       = new collection.mutable.ListBuffer[(Long, String)]
              val rollbackAttrStmtsMap = new java.util.ArrayList[jMap[_, _]]()

              if (curPart != newPart && curPartitions.exists(_._3 == newPart)) {
                Left(s"Partition `$newPart` already exists in database `$db`. Please choose another partition name.")
              } else try {

                // Modifying partition definition for each change
                var updatedPartDef: Part = schema0.parts.find(_.name == curPart).get


                // partition pos ....................................................

                var schema: MetaSchema = if (pos > 0 && pos != curPos) {
                  MetaSchema(schema0.parts.map {
                    case partDef@Part(_, `curPart`, _, _, _) =>
                      // meta
                      meta_Partition(partE).pos(pos).update
                      // client
                      val partDef1 = partDef.copy(pos = pos)
                      updatedPartDef = partDef1
                      partDef1

                    case partDef@Part(otherPos, otherPart, _, _, _) if otherPos > curPos && otherPos <= pos =>
                      // meta
                      val otherPartE = partitionEntities(otherPart)
                      val newPos     = otherPos - 1
                      meta_Partition(otherPartE).pos(newPos).update
                      // client
                      partDef.copy(pos = newPos)

                    case partDef@Part(otherPos, otherPart, _, _, _) if otherPos < curPos && otherPos >= pos =>
                      // meta
                      val otherPartE = partitionEntities(otherPart)
                      val newPos     = otherPos + 1
                      meta_Partition(otherPartE).pos(newPos).update
                      // client
                      partDef.copy(pos = newPos)

                    case partDef =>
                      partDef

                  }.sortBy(_.pos)) // Save partition definitions in new order
                } else schema0


                def updateSchema = {
                  schema = MetaSchema(schema.parts.map {
                    case Part(_, `curPart`, _, _, _) => updatedPartDef
                    case partDef                     => partDef
                  })
                }

                // partition description ....................................................

                if (curDescrOpt != descr) {
                  descr match {
                    case None | Some("") =>
                      meta_Partition(partE).descr().update
                      updatedPartDef = updatedPartDef.copy(descr$ = None)
                    case Some(txt)       =>
                      meta_Partition(partE).descr(txt).update
                      updatedPartDef = updatedPartDef.copy(descr$ = descr)
                  }
                  updateSchema
                }


                // partition name ....................................................

                if (curPart != newPart) {

                  // Alter ident of all attributes in partition (!!)
                  val txMaps = for {
                    Ns(_, ns, _, _, _, attrs) <- updatedPartDef.nss
                    Attr(_, attr, _, _, _, _, _, _, _, _, _, _, _) <- attrs
                  } yield {
                    val (curAttr, newAttr) = (getFullAttr(curPart, ns, attr), getFullAttr(newPart, ns, attr))
                    rollbackAttrStmtsMap.add(
                      Util.map(":db/id", newAttr, ":db/ident", curAttr)
                    )
                    Util.map(":db/id", curAttr, ":db/ident", newAttr)
                  }
                  liveConn.transact(Util.list(txMaps: _*).asInstanceOf[util.List[AnyRef]])

                  // Rename live partition entity itself
                  renameIdent(db, curPart, newPart)

                  // meta
                  meta_Partition(partE).name(newPart).update

                  // update references to all namespaces in this partition
                  val partPrefix: String = curPart + "_"
                  schema = MetaSchema(schema.parts.map {
                    case partDef@Part(_, part, _, _, nss) => {
                      val nsDefs: Seq[Ns] = nss.map {
                        case nsDef@Ns(_, ns, _, _, _, attrs) => {

                          // Update ref attributes
                          val attrDefs: Seq[Attr] = attrs.map {

                            // Ref attribute with reference to old partition name
                            case refAttr@Attr(_, attr, _, _, _, Some(curFullRefNs), _, _, _, _, _, _, _) if curFullRefNs.startsWith(partPrefix) =>

                              // Replace old partition name with new name in reference
                              val newRefNs: String = newPart + "_" + curFullRefNs.split("_")(1)

                              // update meta
                              val attrE: Long = getAttrE(moleculeAdminConn, db, part, ns, attr)
                              // Save original meta refNs for rollback
                              rollbackRefNss += attrE -> curFullRefNs

                              // Save new meta refNs
                              meta_Attribute(attrE).refNs(newRefNs).update

                              // update client
                              refAttr.copy(refNs$ = Some(newRefNs))

                            case otherAttr => otherAttr
                          }
                          nsDef.copy(attrs = attrDefs)
                        }
                      }
                      partDef.copy(nss = nsDefs)
                    }
                  })
                  // re-load updated part with refNss
                  updatedPartDef = schema.parts.find(_.name == curPart).get

                  // client (catch refNs updates too)
                  val nsDefs = updatedPartDef.nss.map(ns => ns.copy(nameFull = s"${newPart}_${ns.name}"))
                  updatedPartDef = updatedPartDef.copy(name = newPart, nss = nsDefs)
                  updateSchema
                }


                // Update def file
                DefFile(db).recreateFrom(schema)

              } catch {
                case error: Throwable => try {
                  // Restore order of meta partitions
                  curPartitions.foreach { case (e, pos1, _) => meta_Partition(e).pos(pos1).update }

                  // Restore meta partition data
                  meta_Partition(partE).name(curPart).descr$(curDescrOpt).update

                  // Restore refNss
                  rollbackRefNss.foreach { case (attrE, refNs) => meta_Attribute(attrE).refNs(refNs).update }

                  // Restore live attribute names
                  if (rollbackAttrStmtsMap.size > 0) {
                    liveConn.transact(rollbackAttrStmtsMap.asInstanceOf[jList[AnyRef]])
                  }

                  // Restore def file
                  DefFile(db).recreateFrom(schema0)

                  Left("Successfully rolled back from error: " + error.getMessage)
                } catch {
                  case rollbackError: Throwable => Left("Unsuccessfully tried to roll back from error! " +
                    "Please check meta db that might be in an invalid state. Unexpected rollback error: " + rollbackError.getMessage)
                }
              }
            }

            case Nil            => Left(s"Couldn't find partition `$curPart` in database `$db`.")
            case morePartitions => Left(s"Unexpectedly found ${morePartitions.size} partitions named `$curPart` in database `$db`.")
          }

          case 0             => Left(s"Couldn't find database `$db`.")
          case databaseCount => Left(s"Unexpectedly found $databaseCount databases named `$db`.")
        }
      }
    }
  }


  def delete(schema: MetaSchema, db: String, part: String): Either[String, MetaSchema] = {
    if (db.isEmpty) {
      Left("Empty db name.")
    } else if (part.isEmpty) {
      Left("Empty partition name.")
    } else {
      implicit val moleculeAdminConn = Conn(base + "/MoleculeAdmin")
      withTransactor {
        meta_Db.name(db).get.size match {
          case 1 => meta_Db.name_(db).Partitions.e.pos.name_(part).descr$.entityCount$.namespaces$.molecules$.get match {

            // Current partition meta data
            case Seq((partE, pos, optDescr, optEntityCound, optNss, optMolecules)) => {
              val liveConn       = Conn(base + "/" + db)
              val metaPartitions = meta_Db.name_(db).Partitions.e.pos.>(pos).get

              val rollbackPartStmtsMap = new java.util.ArrayList[jMap[_, _]]()
              val rollbackAttrStmtsMap = new java.util.ArrayList[jMap[_, _]]()

              try {

                if (optNss.isDefined && optNss.get.nonEmpty) {
                  // Partition has namespace(s)
                  val nss = optNss.get

                  if (meta_Namespace(nss).nameFull.Attrs.name.get.collectFirst {
                    case (nsFull, attr) if getEntityCount(liveConn, s":$nsFull/$attr") > 0 => true
                  }.getOrElse(false)) {
                    // Attribute has attribute with value
                    throw new RuntimeException(
                      s"Couldn't delete partition `$part` in database `$db` having attributes with values. Please delete values first.")
                  }

                  // No values asserted - "park"/rename all attributes in partition (!!)
                  val txMaps = for {
                    Part(_, part1, _, _, nss) <- schema.parts if part1 == part
                    Ns(_, _, nsFull, _, _, attrs) <- nss
                    Attr(_, attr, _, _, _, _, _, _, _, _, _, _, _) <- attrs
                  } yield {
                    rollbackAttrStmtsMap.add(
                      Util.map(":db/id", s":-$nsFull/$attr", ":db/ident", s":$nsFull/$attr")
                    )
                    Util.map(":db/id", s":$nsFull/$attr", ":db/ident", s":-$nsFull/$attr")
                  }
                  liveConn.transact(Util.list(txMaps: _*).asInstanceOf[util.List[AnyRef]])
                }

                // Find live partition entity
                val livePartitionId = liveConn.q(
                  s"""[:find ?livePartId
                     | :where [_ :db.install/partition ?livePartId]
                     |        [?livePartId :db/ident :$part]]""".stripMargin)

                // retract live partition entity if it exists
                if (livePartitionId.nonEmpty)
                  retract(Seq(livePartitionId.head.head.asInstanceOf[Long]))(liveConn)

                rollbackPartStmtsMap.add(Util.map(
                  ":db/ident", s":$part",
                  ":db/id", Peer.tempid(":db.part/db"),
                  ":db.install/_partition", ":db.part/db"
                ))

                // retract meta partition
                partE.retract

                // Shift partition positions af deleted partition
                metaPartitions.foreach { case (e, o) =>
                  val decrPos = o - 1
                  meta_Partition(e).pos(decrPos).update
                }

                // client
                val updatedSchema: MetaSchema = MetaSchema(schema.parts.flatMap {
                  case p if p.pos == pos => Nil
                  case p if p.pos > pos  => Seq(p.copy(pos = p.pos - 1))
                  case p                 => Seq(p)
                })

                // Def file
                DefFile(db).recreateFrom(updatedSchema)

              } catch {

                // Roll-back ==================================================================================
                case error: Throwable => try {

                  // Rollback meta positions for shifted partitions
                  metaPartitions.foreach { case (e, p) => meta_Partition(e).pos(p).update }

                  // Recreate deleted meta partition
                  meta_Partition(partE).pos(pos).name(part).descr$(optDescr).entityCount$(optEntityCound).namespaces$(optNss).molecules$(optMolecules).update

                  // Re-create live partition
                  if (rollbackPartStmtsMap.size > 0) {
                    liveConn.transact(rollbackPartStmtsMap.asInstanceOf[jList[AnyRef]])
                  }

                  // "Un-park" live attribute names
                  if (rollbackAttrStmtsMap.size > 0) {
                    liveConn.transact(rollbackAttrStmtsMap.asInstanceOf[jList[AnyRef]])
                  }

                  // Rollback def file
                  DefFile(db).recreateFrom(schema)

                  Left("Successfully rolled back from error: " + error.getMessage)
                } catch {
                  case rollbackError: Throwable => Left("Unsuccessfully tried to roll back from error! " +
                    "Please check meta db that might be in an invalid state. Unexpected rollback error: " + rollbackError.getMessage)
                }
              }
            }

            case Nil            => Left(s"Couldn't find partition `$part` in database `$db`.")
            case morePartitions => Left(s"Unexpectedly found ${morePartitions.size} partitions named `$part` in database `$db`.")
          }

          case 0              => Left(s"Couldn't find database `$db`.")
          case databasesCount => Left(s"Unexpectedly found $databasesCount databases named `$db`.")
        }
      }
    }
  }


  def renameIdent(db: String, from: String, to: String): Unit = Conn(base + "/" + db).transact(Util.list(
    Util.map(
      ":db/id", s":$from",
      ":db/ident", s":$to"
    )
  ).asInstanceOf[util.List[AnyRef]])
}
