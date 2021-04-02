package moleculeadmin.server.schema

import java.util
import java.util.{List => jList, Map => jMap}
import datomic.Util
import db.admin.dsl.moleculeAdmin._
import molecule.api.out15._
import molecule.facade.Conn
import moleculeadmin.server.Base
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.server.utils.DefFile

object Namespace extends SchemaBase with Base {


  def create(schema: MetaSchema, db: String, part: String, nsOnly: String, descr: Option[String] = None, pos0: Int): Either[String, MetaSchema] = {
    if (db.isEmpty) {
      Left("Empty db name.")
    } else if (part.isEmpty) {
      Left("Empty partition name.")
    } else if (nsOnly.isEmpty) {
      Left("Empty namespace name.")
    } else if (nsOnly.head.isLower) {
      Left("Attribute name should start with uppercase letter and match `[A-Z][a-zA-Z0-9]*`.")
    } else if (!nsOnly.matches("[A-Z][a-zA-Z0-9]*")) {
      Left("Attribute name should match `[A-Z][a-zA-Z0-9]*`.")
    } else {
      implicit val moleculeAdminConn = Conn(base + "/MoleculeAdmin")
      withTransactor {
        meta_Db.e.name_(db).get.size match {
          case 1 => meta_Db.name_(db).Partitions.e.name_(part).get match {
            case Seq(partE) => {
              val nsFull              = getFullNs(part, nsOnly)
              val curNamespaces       = meta_Partition(partE).Namespaces.e.pos.name.get
              val pos                 = if (pos0 == 0) curNamespaces.length + 1 else pos0
              var newNamespaceE: Long = 0L

              if (curNamespaces.exists(_._3 == nsOnly)) {
                Left(s"Attribute `$nsOnly` already exists in partition `$part` in database `$db`. Please choose another namespace name.")
              } else try {

                // Add new ns to meta partition
                newNamespaceE = meta_Namespace.pos(pos).name(nsOnly).nameFull(nsFull).save.eid
                meta_Partition(partE).namespaces.assert(newNamespaceE).update

                // Increase pos index for namespaces after new namespace
                curNamespaces.filter(_._2 >= pos).foreach { case (e, p, _) =>
                  val incrPos = p + 1
                  meta_Namespace(e).pos(incrPos).update
                }

                // Add ns to client schema
                val updatedSchema: MetaSchema = MetaSchema(schema.parts.map {
                  case p if p.name == part =>
                    val newNamespace            = MetaNs(pos, nsOnly, nsFull, descr, None, Nil)
                    val updatedNss: Seq[MetaNs] = if (p.nss.isEmpty) {
                      Seq(newNamespace)
                    } else {
                      p.nss.flatMap {
                        case ns1 if ns1.pos == 1 && pos == 1 => Seq(newNamespace, ns1.copy(pos = ns1.pos + 1))
                        case ns1 if ns1.pos == pos - 1       => Seq(ns1, newNamespace)
                        case ns1 if ns1.pos < pos - 1        => Seq(ns1)
                        case ns1                             => Seq(ns1.copy(pos = ns1.pos + 1))
                      }
                    }
                    p.copy(nss = updatedNss)
                  case otherPartition      => otherPartition
                })

                // Add ns to def file
                DefFile(db).recreateFrom(updatedSchema)

              } catch {
                case error: Throwable => try {
                  // Restore namespace order
                  curNamespaces.filter(_._2 >= pos).foreach { case (e, n, _) => meta_Namespace(e).pos(n).update }

                  // Delete obsolete new meta namespace
                  if (newNamespaceE > 0L) newNamespaceE.retract

                  // Restore def file
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

          case 0             => Left(s"Couldn't find database `$db`.")
          case databaseCount => Left(s"Unexpectedly found $databaseCount databases named `$db`.")
        }
      }
    }
  }


  def update(schema0: MetaSchema, db: String, part: String, currentNs: String, newNs: String, descr: Option[String], pos0: Int): Either[String, MetaSchema] = {
    if (newNs.isEmpty) {
      Left("Empty new namespace name.")
    } else if (newNs.head.isLower) {
      Left("Attribute name should start with uppercase letter and match `[A-Z][a-zA-Z0-9]*`.")
    } else if (!newNs.matches("[A-Z][a-zA-Z0-9]*")) {
      Left("Attribute name should match `[A-Z][a-zA-Z0-9]*`.")
    } else {
      implicit val moleculeAdminConn = Conn(base + "/MoleculeAdmin")
      withTransactor {
        meta_Db.e.name_(db).get.size match {
          case 1 => meta_Db.name_(db).Partitions.name(part).get.size match {
            case 1 => meta_Db.name_(db).Partitions.name_(part).Namespaces.e.pos.name_(currentNs).nameFull.descr$.get match {

              // Current namespace data
              case Seq((nsE, curPos, curNsFull, curDescrOpt)) => {
                val liveConn = Conn(base + "/" + db)

                // All namespaces
                val metaNss                       = meta_Db.name_(db).Partitions.name_(part).Namespaces.e.pos.name.get
                val pos                           = if (pos0 == 0) curPos else pos0
                val nsEntities: Map[String, Long] = metaNss.map(a => a._3 -> a._1).toMap

                val rollbackRefNss       = new collection.mutable.ListBuffer[(Long, String)]
                val rollbackAttrStmtsMap = new java.util.ArrayList[jMap[_, _]]()


                if (currentNs != newNs && metaNss.map(_._3).contains(newNs)) {
                  Left(s"Attribute `$newNs` already exists in partition `$part` in database `$db`. Please choose another namespace name.")
                } else try {

                  // Modifying ns definition for each change
                  var updatedNsDef: MetaNs = schema0.parts.find(_.name == part).get.nss.find(_.name == currentNs).get


                  // namespace pos ....................................................

                  var schema: MetaSchema = if (pos > 0 && pos != curPos) {
                    MetaSchema(schema0.parts.map {
                      case partDef@MetaPart(_, `part`, _, _, nss) => {
                        val nsDefs: Seq[MetaNs] = nss.map {
                          case nsDef@MetaNs(_, `currentNs`, _, _, _, _) =>
                            // meta
                            meta_Namespace(nsE).pos(pos).update
                            // client
                            val nsDef1 = nsDef.copy(pos = pos)
                            updatedNsDef = nsDef1
                            nsDef1

                          case nsDef@MetaNs(otherPos, otherNs, _, _, _, _) if otherPos > curPos && otherPos <= pos =>
                            // meta
                            val otherNsE = nsEntities(otherNs)
                            val newPos   = otherPos - 1
                            meta_Namespace(otherNsE).pos(newPos).update
                            // client
                            nsDef.copy(pos = newPos)

                          case nsDef@MetaNs(otherPos, otherNs, _, _, _, _) if otherPos < curPos && otherPos >= pos =>
                            // meta
                            val otherNsE = nsEntities(otherNs)
                            val newPos   = otherPos + 1
                            meta_Namespace(otherNsE).pos(newPos).update
                            // client
                            nsDef.copy(pos = newPos)

                          case nsDef => nsDef

                        }.sortBy(_.pos) // Save namespace definitions in new order

                        // Update modified namespace definitions
                        partDef.copy(nss = nsDefs)
                      }

                      case partDef => partDef
                    })
                  } else schema0


                  def updateSchema = {
                    schema = MetaSchema(schema.parts.map {
                      case partDef@MetaPart(_, `part`, _, _, nss) => {
                        val nsDefs: Seq[MetaNs] = nss.map {
                          case MetaNs(_, `currentNs`, _, _, _, _) => updatedNsDef
                          case nsDef1                             => nsDef1
                        }
                        partDef.copy(nss = nsDefs)
                      }

                      case partDef => partDef
                    })
                  }

                  // namespace description ....................................................

                  if (curDescrOpt != descr) {
                    descr match {
                      case None | Some("") =>
                        meta_Namespace(nsE).descr().update
                        updatedNsDef = updatedNsDef.copy(descr$ = None)
                      case Some(txt)       =>
                        meta_Namespace(nsE).descr(txt).update
                        updatedNsDef = updatedNsDef.copy(descr$ = descr)
                    }
                    updateSchema
                  }


                  // namespace name ....................................................

                  if (currentNs != newNs) {

                    // Alter ident of all attributes in namespace (!)
                    val txMaps = updatedNsDef.attrs.map { attr =>
                      val (curAttr, newAttr) = (getFullAttr(part, currentNs, attr.name), getFullAttr(part, newNs, attr.name))
                      rollbackAttrStmtsMap.add(
                        Util.map(":db/id", newAttr, ":db/ident", curAttr)
                      )
                      Util.map(":db/id", curAttr, ":db/ident", newAttr)
                    }
                    liveConn.transact(Util.list(txMaps: _*).asInstanceOf[util.List[AnyRef]])

                    val curNsFull: String = getFullNs(part, currentNs)
                    val newNsFull: String = getFullNs(part, newNs)

                    // update meta references to this ns
                    schema = MetaSchema(schema.parts.map {
                      case partDef@MetaPart(_, part1, _, _, nss) => {
                        val nsDefs: Seq[MetaNs] = nss.map {
                          case nsDef@MetaNs(_, ns, nsFull, _, _, attrs) => {
                            val attrDefs: Seq[MetaAttr] = attrs.map {

                              case refAttr@MetaAttr(_, attr, _, _, _, Some(`curNsFull`), _, _, _, _, _, _, _) =>

                                // update meta
                                val attrE: Long = getAttrE(moleculeAdminConn, db, part1, ns, attr)
                                // Save original meta refNs for rollback
                                rollbackRefNss += attrE -> curNsFull

                                // Save new meta refNs
                                meta_Attribute(attrE).refNs(newNsFull).update

                                // update client
                                refAttr.copy(refNs$ = Some(newNsFull))

                              case otherAttr =>
                                otherAttr
                            }
                            nsDef.copy(attrs = attrDefs)
                          }
                        }
                        partDef.copy(nss = nsDefs)
                      }
                    })
                    // re-load updated namespace with refNss
                    updatedNsDef = schema.parts.find(_.name == part).get.nss.find(_.name == currentNs).get

                    // Update meta name
                    meta_Namespace(nsE).name(newNs).nameFull(newNsFull).update

                    // client (get refNs updates too)
                    updatedNsDef = updatedNsDef.copy(name = newNs, nameFull = newNsFull)
                    updateSchema
                  }


                  // Update def file
                  DefFile(db).recreateFrom(schema)

                } catch {
                  case error: Throwable => try {
                    // Restore order of meta namespaces
                    metaNss.foreach { case (e, pos1, _) => meta_Namespace(e).pos(pos1).update }

                    // Restore meta namespace data
                    meta_Namespace(nsE).name(currentNs).nameFull(curNsFull).descr$(curDescrOpt).update

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

              case Nil            => Left(s"Couldn't find current namespace `$currentNs` in partition `$part` in database `$db`.")
              case moreNamespaces => Left(s"Unexpectedly found ${moreNamespaces.size} namespaces named `$currentNs` in partition `$part` in database `$db`.")
            }

            case 0               => Left(s"Couldn't find partition `$part` in database `$db`.")
            case partitionsCount => Left(s"Unexpectedly found $partitionsCount partitions named `$part` in database `$db`.")
          }

          case 0             => Left(s"Couldn't find database `$db`.")
          case databaseCount => Left(s"Unexpectedly found $databaseCount databases named `$db`.")
        }
      }
    }
  }


  def delete(schema: MetaSchema, db: String, part: String, nsOnly: String): Either[String, MetaSchema] = {
    if (db.isEmpty) {
      Left("Empty db name.")
    } else if (part.isEmpty) {
      Left("Empty partition name.")
    } else if (nsOnly.isEmpty) {
      Left(s"Empty namespace name.")
    } else {
      implicit val conn = Conn(base + "/MoleculeAdmin")
      withTransactor {
        meta_Db.e.name_(db).get.size match {
          case 1 => meta_Db.name_(db).Partitions.name(part).get.size match {
            case 1 => meta_Db.name_(db).Partitions.name_(part).Namespaces.e.pos.name_(nsOnly).nameFull.descr$.attrs$.get match {

              // Current namespace meta data
              case Seq((nsE, pos, nsFull, optDescr, optAttrs)) => {
                val liveConn       = Conn(base + "/" + db)
                val metaNamespaces = meta_Db.name_(db).Partitions.name_(part).Namespaces.e.pos.>(pos).get

                val rollbackAttrStmtsMap = new java.util.ArrayList[jMap[_, _]]()

                try {

                  if (optAttrs.isDefined && optAttrs.get.nonEmpty) {
                    // Attribute has attribute(s)
                    val attrs = optAttrs.get

                    if (meta_Attribute(attrs).name.get.collectFirst {
                      case attr if getEntityCount(liveConn, s":$nsFull/$attr") > 0 => true
                    }.getOrElse(false)) {
                      // Attribute has attribute with value
                      throw new RuntimeException(
                        s"Couldn't delete namespace `$nsOnly` in partition `$part` in database `$db` having attributes with values. Please delete values first.")
                    }

                    // No values asserted - "park"/rename all attributes in namespace (!)
                    val txMaps = for {
                      MetaPart(_, part1, _, _, nss) <- schema.parts if part1 == part
                      MetaNs(_, ns1, nsFull, _, _, attrs) <- nss if ns1 == nsOnly
                      MetaAttr(_, attr, _, _, _, _, _, _, _, _, _, _, _) <- attrs
                    } yield {
                      rollbackAttrStmtsMap.add(
                        Util.map(":db/id", s":-$nsFull/$attr", ":db/ident", s":$nsFull/$attr")
                      )
                      Util.map(":db/id", s":$nsFull/$attr", ":db/ident", s":-$nsFull/$attr")
                    }
                    liveConn.transact(Util.list(txMaps: _*).asInstanceOf[util.List[AnyRef]])
                  }

                  // retract meta namespace
                  nsE.retract

                  // Shift namespace positions af deleted namespace
                  metaNamespaces.foreach { case (e, o) =>
                    val decrPos = o - 1
                    meta_Namespace(e).pos(decrPos).update
                  }

                  // client
                  val updatedSchema: MetaSchema = MetaSchema(schema.parts.map {
                    case p if p.name == part => {
                      val updatedNss = p.nss.flatMap {
                        case ns1 if ns1.pos == pos => Nil
                        case ns1 if ns1.pos > pos  => Seq(ns1.copy(pos = ns1.pos - 1))
                        case ns1                   => Seq(ns1)
                      }
                      p.copy(nss = updatedNss)
                    }
                    case p                   => p
                  })

                  // Def file
                  DefFile(db).recreateFrom(updatedSchema)

                } catch {

                  // Roll-back ==================================================================================
                  case error: Throwable => try {

                    // Rollback meta positions for shifted namespaces in partition
                    metaNamespaces.foreach { case (e, pos) => meta_Namespace(e).pos(pos).update }

                    // Recreate deleted meta namespace
                    meta_Namespace(nsE).pos(pos).name(nsOnly).nameFull(nsFull).descr$(optDescr).attrs$(optAttrs).update

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

              case Nil            => Left(s"Couldn't find namespace `$nsOnly` in partition `$part` in database `$db`.")
              case moreNamespaces => Left(s"Unexpectedly found ${moreNamespaces.size} namespaces named `$nsOnly` in partition `$part` in database `$db`.")
            }

            case 0               => Left(s"Couldn't find partition `$part` in database `$db`.")
            case partitionsCount => Left(s"Unexpectedly found $partitionsCount partitions named `$part` in database `$db`.")
          }

          case 0             => Left(s"Couldn't find database `$db`.")
          case databaseCount => Left(s"Unexpectedly found $databaseCount databases named `$db`.")
        }
      }
    }
  }
}