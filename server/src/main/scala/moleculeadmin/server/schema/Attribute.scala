package moleculeadmin.server.schema

import java.net.URI
import java.util
import java.util.{UUID, List => jList, Map => jMap}
import datomic.{Peer, Util}
import db.admin.dsl.meta._
import molecule.api.out15._
import molecule.facade.Conn
import moleculeadmin.server.Base
import moleculeadmin.shared.ast.schema._
import moleculeadmin.server.utils.DefFile
import scala.collection.JavaConverters._


object Attribute extends SchemaBase  with Base {


  def create(schema: MetaSchema, db: String, part: String, nsOnly: String, attr: String, card: Int, tpe: String, enums: Seq[String] = Nil,
             optRefNs: Option[String] = None, options: Seq[String] = Nil, doc: Option[String] = None, pos0: Int = 0): Either[String, MetaSchema] = {
    if (db.isEmpty) {
      Left(s"Empty db name.")
    } else if (part.isEmpty) {
      Left(s"Empty partition name.")
    } else if (nsOnly.isEmpty) {
      Left(s"Empty namespace name.")
    } else if (attr.isEmpty) {
      Left(s"Empty attribute name.")
    } else if (attr.head.isUpper) {
      Left("Attribute name should start with lowercase letter and match `[a-z][a-zA-Z0-9]*`.")
    } else if (!attr.matches("[a-z][a-zA-Z0-9]*")) {
      Left("Attribute name should match `[a-z][a-zA-Z0-9]*`.")
    } else if (reservedAttrNames.contains(attr)) {
      Left("Reserved attribute names:\n" + reservedAttrNames.mkString("\n"))
    } else {
      implicit val metaConn = Conn(base + "/meta")
      withTransactor {
        meta_Db.e.name_(db).get.size match {
          case 1 => meta_Db.name_(db).Partitions.e.name_(part).get.size match {
            case 1 => meta_Db.name_(db).Partitions.name_(part).Namespaces.e.name_(nsOnly).get match {
              case Seq(nsE) => {
                val liveConn = Conn(base + "/" + db)
                val enums1   = if (enums.nonEmpty) Some(enums.toSet) else None
                val options1 = if (options.nonEmpty) Some(options.toSet) else None

                val curAttrs       = meta_Namespace(nsE).Attrs.e.pos.name.get
                val pos            = if (pos0 == 0) curAttrs.length + 1 else pos0
                var newAttrE: Long = 0L
                val fullNs         = getFullNs(part, nsOnly)

                val rollbackAttrStmtsMap = new java.util.ArrayList[jMap[_, _]]()

                if (curAttrs.exists(_._3 == attr)) {
                  Left(s"Attribute `$attr` already exists in namespace `$nsOnly` in partition `$part` in database `$db`. Please choose another attribute name.")
                } else if (optRefNs.isDefined && !schema.parts.flatMap(_.nss).map(_.nameFull).contains(optRefNs.get)) {
                  Left(s"Couldn't find ref namespace `${optRefNs.get}` in client schema.")
                } else try {

                  // Add attr to namespace in meta schema
                  newAttrE = meta_Attribute.pos(pos).name(attr).card(card).tpe(tpe).enums$(enums1).refNs$(optRefNs).options$(options1).doc$(doc).save.eid
                  meta_Namespace(nsE).attrs.assert(newAttrE).update

                  // Increase position for attributes after new attribute
                  curAttrs.filter(_._2 >= pos).foreach { case (e, n, _) =>
                    val incrPos = n + 1
                    meta_Attribute(e).pos(incrPos).update
                  }

                  // Add attr to live schema
                  addAttrToLiveSchema(liveConn, part, nsOnly, attr, card, tpe, enums, options, doc)
                  rollbackAttrStmtsMap.add(
                    Util.map(":db/id", s":$fullNs/$attr", ":db/ident", s":-$fullNs/$attr")
                  )

                  // Add attr to client schema
                  val updatedSchema: MetaSchema = MetaSchema(schema.parts.map {
                    case p if p.name == part =>
                      val updatedNss: Seq[Ns] = p.nss.map {
                        case ns1 if ns1.name == nsOnly =>
                          val newAttr     : Attr      = Attr(pos, attr, card, tpe, enums1, optRefNs, options1, doc, None, None, None, None, Nil)
                          val updatedAttrs: Seq[Attr] = if (ns1.attrs.isEmpty) {
                            Seq(newAttr)
                          } else {
                            ns1.attrs.flatMap {
                              case a if a.pos == 1 && pos == 1 => Seq(newAttr, a.copy(pos = a.pos + 1))
                              case a if a.pos == pos - 1       => Seq(a, newAttr)
                              case a if a.pos < pos - 1        => Seq(a)
                              case a                           => Seq(a.copy(pos = a.pos + 1))
                            }
                          }
                          ns1.copy(attrs = updatedAttrs)
                        case otherNamespace        => otherNamespace
                      }
                      p.copy(nss = updatedNss)
                    case otherPartition      => otherPartition
                  })

                  // Add attr to def file
                  DefFile(db).recreateFrom(updatedSchema)

                } catch {

                  // Roll-back ==================================================================================
                  case error: Throwable => try {

                    // Restore attribute order
                    curAttrs.filter(_._2 >= pos).foreach { case (e, n, _) => meta_Attribute(e).pos(n).update }

                    // Retract obsolete new meta attribute
                    if (newAttrE > 0L) newAttrE.retract

                    // "Park" new obsolete live attribute name
                    if (rollbackAttrStmtsMap.size > 0) {
                      liveConn.transact(rollbackAttrStmtsMap.asInstanceOf[jList[AnyRef]])
                    }

                    // Restore def file
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


  def update(schema0: MetaSchema, db: String, part: String, nsOnly: String, curAttr: String, newAttr: String, card: Int, tpe: String, enums: Seq[String],
             optRefNs: Option[String], options: Seq[String], doc: Option[String], pos0: Int, attrGroup: Option[String]): Either[String, MetaSchema] = {
    if (db.isEmpty) {
      Left(s"Empty db name.")
    } else if (part.isEmpty) {
      Left(s"Empty partition name.")
    } else if (nsOnly.isEmpty) {
      Left(s"Empty namespace name.")
    } else if (newAttr.isEmpty) {
      Left(s"Empty attribute name.")
    } else if (newAttr.head.isUpper) {
      Left("Attribute name should start with lowercase letter and match `[a-z][a-zA-Z0-9]*`.")
    } else if (!newAttr.matches("[a-z][a-zA-Z0-9]*")) {
      Left("Attribute name should match `[a-z][a-zA-Z0-9]*`.")
    } else if (reservedAttrNames.contains(newAttr)) {
      Left("Reserved attribute names:\n" + reservedAttrNames.mkString("\n"))
    } else if (!List(1, 2, 3).contains(card)) {
      Left(s"Cardinality number can only be 1, 2 or 3. Found: `$card`")
    } else if (part.nonEmpty && !schema0.parts.exists(_.name == part)) {
      Left(s"Couldn't find partition `$part` in client schema.")
    } else if (!schema0.parts.find(_.name == part).get.nss.exists(_.name == nsOnly)) {
      Left(s"Couldn't find namespace `$nsOnly` in partition `$part` in client schema.")
    } else if (!schema0.parts.find(_.name == part).get.nss.find(_.name == nsOnly).get.attrs.exists(_.name == curAttr)) {
      Left(s"Couldn't find attribute `$curAttr` in namespace `$nsOnly` in partition `$part` in client schema.")
    } else {
      implicit val metaConn = Conn(base + "/meta")
      withTransactor {
        meta_Db.e.name_(db).get.size match {
          case 1 => meta_Db.name_(db).Partitions.e.name_(part).get.size match {
            case 1 => meta_Db.name_(db).Partitions.name_(part).Namespaces.e.name_(nsOnly).get.size match {
              case 1 => meta_Db.name_(db).Partitions.name_(part).Namespaces.name_(nsOnly).Attrs.e.name_(curAttr).pos.card.tpe.enums$.refNs$.options$.doc$.attrGroup$.get match {

                // Current attribute data
                case Seq((attrE, curPos, curCard, curTpe, curEnums, curRefNs, curOptions0, curDoc, curAttrGroup)) => {

                  val curOptions: Seq[String] = curOptions0.getOrElse(Nil).toSeq.filterNot(_ == "indexed").sorted

                  // All attributes
                  val metaAttrs   : List[(Long, Int, String)] = meta_Namespace.name_(nsOnly).Attrs.e.pos.name.get
                  val attrEntities: Map[String, Long]         = metaAttrs.map(a => a._3 -> a._1).toMap

                  val nsFull      = getFullNs(part, nsOnly)
                  val curAttrFull = getFullAttr(part, nsOnly, curAttr)
                  val pos         = if (pos0 == 0) curPos else pos0


                  // Rollbacks

                  val rollbackCardStmtsList: java.util.ArrayList[jList[_]]   = new java.util.ArrayList[jList[_]]()
                  val rollbackCardStmtsMap : java.util.ArrayList[jMap[_, _]] = new java.util.ArrayList[jMap[_, _]]()

                  val rollbackOptionsStmtsList: java.util.ArrayList[jList[_]]   = new java.util.ArrayList[jList[_]]()
                  val rollbackOptionsStmtsMap : java.util.ArrayList[jMap[_, _]] = new java.util.ArrayList[jMap[_, _]]()

                  val rollbackDocStmtsList: java.util.ArrayList[jList[_]]   = new java.util.ArrayList[jList[_]]()
                  val rollbackDocStmtsMap : java.util.ArrayList[jMap[_, _]] = new java.util.ArrayList[jMap[_, _]]()

                  val rollbackAttrStmtsMap: java.util.ArrayList[jMap[_, _]] = new java.util.ArrayList[jMap[_, _]]()


                  val liveConn = Conn(base + "/" + db)

                  if (curAttr != newAttr && metaAttrs.map(_._3).contains(newAttr)) {
                    Left(s"Attribute `$newAttr` already exists in namespace `$nsOnly` in partition `$part` in database `$db`. Please choose another attribute name.")
                  } else try {

                    // Modifying attribute definition for each change
                    def getUpdatedAttrDef(sch: MetaSchema) = sch.parts.find(_.name == part) match {
                      case Some(partition) => partition.nss.find(_.name == nsOnly) match {
                        case Some(namespace) => namespace.attrs.find(_.name == curAttr) match {
                          case Some(attribute) => attribute
                          case None            => throw new RuntimeException(s"Couldn't find attribute `$curAttr` in namespace `$nsOnly` in partition `$part` of client schema. " +
                            s"Attribute `$nsOnly` contains the following attribute definitions:\n" + namespace.attrs.mkString("\n"))
                        }
                        case None            => throw new RuntimeException(s"Couldn't find namespace `$nsOnly` in partition `$part` of client schema. " +
                          s"Partition `$part` contains the following namespace definitions:\n" + partition.nss.mkString("\n"))
                      }
                      case None            => throw new RuntimeException(s"Couldn't find partition `$part` in client schema. " +
                        "Schema contains the following partition definitions:\n" + schema0.parts.mkString("\n"))
                    }

                    var updatedAttrDef: Attr = getUpdatedAttrDef(schema0)


                    // attribute pos ............................................................................

                    var schema: MetaSchema = if (pos > 0 && pos != curPos) {
                      MetaSchema(schema0.parts.map {
                        case partDef@Part(_, `part`, _, _, nss) => {
                          val nsDefs: Seq[Ns] = nss.map {
                            case nsDef@Ns(_, `nsOnly`, _, _, _, attrs) => {
                              val attrDefs: Seq[Attr] = attrs.map {
                                // Current attr
                                case attrDef@Attr(_, `curAttr`, _, _, _, _, _, _, _, _, _, _, _) =>
                                  // meta
                                  meta_Attribute(attrE).pos(pos).update
                                  // client
                                  val attrDef1 = attrDef.copy(pos = pos)
                                  updatedAttrDef = attrDef1
                                  updatedAttrDef

                                // Attrs before
                                case attrDef@Attr(otherPos, otherAttr, _, _, _, _, _, _, _, _, _, _, _) if otherPos > curPos && otherPos <= pos =>
                                  // meta
                                  val otherAttrE = attrEntities(otherAttr)
                                  val newPos     = otherPos - 1
                                  meta_Attribute(otherAttrE).pos(newPos).update
                                  // client
                                  attrDef.copy(pos = newPos)

                                // Attrs after
                                case attrDef@Attr(otherPos, otherAttr, _, _, _, _, _, _, _, _, _, _, _) if otherPos < curPos && otherPos >= pos =>
                                  // meta
                                  val otherAttrE = attrEntities(otherAttr)
                                  val newPos     = otherPos + 1
                                  meta_Attribute(otherAttrE).pos(newPos).update
                                  // client
                                  attrDef.copy(pos = newPos)

                                case attrDef => attrDef

                              }.sortBy(_.pos) // Save attribute definitions in new order

                              // Update modified attribute definitions
                              nsDef.copy(attrs = attrDefs)
                            }

                            case nsDef => nsDef
                          }

                          // Update modified namespace definitions
                          partDef.copy(nss = nsDefs)
                        }

                        case partDef => partDef
                      })
                    } else schema0
                    updatedAttrDef = getUpdatedAttrDef(schema)


                    // attribute card .............................................................................

                    if (curCard != card) {

                      def stripKeys(): Unit = {
                        // strip keys from all values (!)
                        val stripKey: AnyRef => Any = tpe match {
                          case "String"         => v: AnyRef => v.toString.split("@", 2)(1)
                          case "Int"            => v: AnyRef => v.toString.split("@", 2)(1).toLong
                          case "Float"          => v: AnyRef => v.toString.split("@", 2)(1).toDouble
                          case "Boolean"        => v: AnyRef => v.toString.split("@", 2)(1).toBoolean
                          case "Long"           => v: AnyRef => v.toString.split("@", 2)(1).toLong
                          case "Double"         => v: AnyRef => v.toString.split("@", 2)(1).toDouble
//                          case "java.util.Date" => v: AnyRef => datomicStr2date(v.toString.split("@", 2)(1))
                          case "java.util.Date" => v: AnyRef => str2date(v.toString.split("@", 2)(1))
                          case "java.util.UUID" => v: AnyRef => UUID.fromString(v.toString.split("@", 2)(1))
                          case "java.net.URI"   => v: AnyRef => new URI(v.toString.split("@", 2)(1))
                          case "BigInt"         => v: AnyRef => BigInt(v.toString.split("@", 2)(1))
                          case "BigDecimal"     => v: AnyRef => BigDecimal(v.toString.split("@", 2)(1))
                        }
                        val attr                    = s":$nsFull/$curAttr"
                        val rows                    = liveConn.qRaw(s"[:find ?e ?v :where [?e $attr ?v]]")
                        val stmts                   = new java.util.ArrayList[jList[_]](rows.size)
                        val it                      = rows.iterator
                        var row     : jList[AnyRef] = null
                        var eid     : AnyRef        = null
                        var keyValue: AnyRef        = null
                        var value   : AnyRef        = null
                        while (it.hasNext) {
                          row = it.next()
                          eid = row.get(0)
                          keyValue = row.get(1)
                          value = stripKey(keyValue).asInstanceOf[AnyRef]
                          stmts.add(Util.list(":db/retract", eid, attr, keyValue))
                          stmts.add(Util.list(":db/add", eid, attr, value))
                          // Prepare roll-back statements if needed
                          rollbackCardStmtsList.add(Util.list(":db/retract", eid, attr, value))
                          rollbackCardStmtsList.add(Util.list(":db/add", eid, attr, keyValue))
                        }
                        // todo: throttle 1000 stmts at a time in parallel
                        liveConn.transact(stmts.asInstanceOf[jList[AnyRef]])
                      }

                      // re-load updated schema
                      updatedAttrDef = getUpdatedAttrDef(schema)

                      card match {
                        case 1 =>

                          val maxCardManyValueCount = {
                            val valueCounts = liveConn.q("[:find (max ?count) :in $$ [[?e ?count]]]",
                              liveConn.qRaw(s"[:find ?e (count ?v) :where [?e :$nsFull/$curAttr ?v]]"))

                            if (valueCounts.nonEmpty && valueCounts.head.nonEmpty)
                              valueCounts.head.head.asInstanceOf[Int]
                            else 0
                          }

                          if (maxCardManyValueCount > 1)
                            throw new RuntimeException("Couldn't change attribute to cardinality 1 since some entities have multiple values. " +
                              "Please reduce all values to one value before changing cardinality.")

                          // live
                          if (curCard == 3) {
                            if (curTpe == "String") {
                              // Change cardinality of current attribute
                              liveConn.transact(Util.list(Util.map(":db/id", s":$nsFull/$curAttr", ":db/cardinality", ":db.cardinality/one")).asInstanceOf[util.List[AnyRef]])
                              rollbackCardStmtsMap.add(Util.map(":db/id", s":$nsFull/$curAttr", ":db/cardinality", ":db.cardinality/many"))

                            } else {
                              throw new RuntimeException("Changing map cardinality when not of type String not supported yet " +
                                "(would require creating a new attribute of the target type).")

                              // todo?
                              //                            // Create new attribute with current name since we can't alter the map type of String to the new type
                              //                            addAttrToLiveSchema(liveConn, part, ns, curAttr, 1, tpe, curEnums.getOrElse(Nil).toSeq, curOptions.getOrElse(Nil).toSeq, curDoc)
                            }
                            stripKeys()
                          } else {
                            // Current card 2
                            liveConn.transact(Util.list(Util.map(":db/id", s":$nsFull/$curAttr", ":db/cardinality", ":db.cardinality/one")).asInstanceOf[util.List[AnyRef]])
                            rollbackCardStmtsMap.add(Util.map(":db/id", s":$nsFull/$curAttr", ":db/cardinality", ":db.cardinality/many"))
                          }

                          meta_Attribute(attrE).card(1).update
                          updatedAttrDef = updatedAttrDef.copy(card = 1)

                        case 2 =>
                          // live
                          if (curCard == 3) {
                            if (curTpe == "String") {
                              // Create new attribute with current name since we can't alter the map type of String to the new type
                              addAttrToLiveSchema(liveConn, part, nsOnly, curAttr, 2, tpe, curEnums.getOrElse(Nil).toSeq, curOptions, curDoc)
                            } else {
                              throw new RuntimeException("Changing map cardinality when not of type String not supported yet " +
                                "(would require creating a new attribute of the target type).")
                            }
                            stripKeys()
                          } else {
                            // Current card 1
                            liveConn.transact(Util.list(Util.map(":db/id", s":$nsFull/$curAttr", ":db/cardinality", ":db.cardinality/many")).asInstanceOf[util.List[AnyRef]])
                            rollbackCardStmtsMap.add(Util.map(":db/id", s":$nsFull/$curAttr", ":db/cardinality", ":db.cardinality/one"))
                          }

                          meta_Attribute(attrE).card(2).update
                          updatedAttrDef = updatedAttrDef.copy(card = 2)


                        case 3 => // Map attribute
                          throw new RuntimeException("Couldn't change to map cardinality since keys are unknown. Please create a new map attribute and populate with keyed data.")
                      }
                    }


                    // attribute enums ............................................................................

                    if (tpe == "Enum") {

                      if (enums.isEmpty)
                        throw new RuntimeException("No enum values passed.")

                      val curEnumValues   : Seq[String] = curEnums.getOrElse(Nil).toSeq.sorted
                      val passedEnumValues: Seq[String] = enums.sorted
                      val liveEnums       : Seq[String] = Schema.a_(curAttrFull).enum.get(liveConn).sorted

                      // Check that current client schema enums are in sync with live enums
                      if (curEnumValues != liveEnums)
                        throw new RuntimeException(
                          s"""Client schema enums not in sync with live enums:
                             |Client enums: ${curEnumValues.mkString(", ")}
                             |Live enums  : ${liveEnums.mkString(", ")}
                        """.stripMargin
                        )

                      // Updated enums
                      if (curEnumValues != passedEnumValues) {

                        if (curEnumValues.isEmpty)
                          throw new RuntimeException(s"Passed list of enum values can't be empty.")

                        val enumNs = getFullNs(part, nsOnly)

                        val obsoleteEnums: Seq[String] = curEnumValues.diff(passedEnumValues)
                        val newEnums                   = passedEnumValues.diff(curEnumValues)

                        val liveEnumValues: Seq[String] = {
                          val result = liveConn.q(s"[:find (distinct ?enum) :where [?e :$enumNs/$curAttr ?ref] [?ref :db/ident ?ident] [(name ?ident) ?enum]]")
                          if (result.isEmpty) Nil else result.head.head.asInstanceOf[Set[String]].toSeq
                        }

                        val conflictingObsoleteEnums: Seq[String] = obsoleteEnums.intersect(liveEnumValues)

                        if (conflictingObsoleteEnums.nonEmpty)
                          throw new RuntimeException(s"Couldn't remove obsolete enums having live values. Please remove values before removing enums from schema.\n" +
                            "Conflicting obsolete enum values: " + conflictingObsoleteEnums.mkString(", ")
                          )

                        // "park" obsolete enum values - todo: necessary?
                        val parkStmts: Seq[jMap[_, _]] = obsoleteEnums.map { obsoleteEnum =>
                          Util.map(":db/id", s":$enumNs.$curAttr/$obsoleteEnum", ":db/ident", s":-$enumNs.$curAttr/$obsoleteEnum")
                        }

                        // Add new enums
                        val addStmts: Seq[jMap[_, _]] = newEnums.map { newEnum =>
                          Util.map(":db/id", Peer.tempid(s":$part"), ":db/ident", s":$enumNs.$curAttr/$newEnum")
                        }

                        // live
                        liveConn.transact(Util.list(parkStmts ++ addStmts: _*).asInstanceOf[util.List[AnyRef]])

                        // meta
                        meta_Attribute(attrE).enums(passedEnumValues).update

                        // client
                        updatedAttrDef = updatedAttrDef.copy(enums$ = Some(passedEnumValues.toSet))
                      }
                    }


                    // attribute refNs ............................................................................

                    if (tpe == "ref" && optRefNs.isDefined && curRefNs != optRefNs) {

                      // Check that ref ns exists
                      val refNs = optRefNs.get
                      if (!schema.parts.flatMap(_.nss).map(_.nameFull).contains(refNs))
                        throw new RuntimeException(s"Couldn't find ref namespace `$refNs` in client schema.")

                      // meta
                      meta_Attribute(attrE).refNs(refNs).update

                      // client
                      updatedAttrDef = updatedAttrDef.copy(refNs$ = optRefNs)
                    }


                    // attribute options ..........................................................................

                    if (curOptions != options.sorted) {

                      if (options.contains("fulltext"))
                        throw new RuntimeException(s"Can't add fulltext option to existing attribute.")

                      val passedOpts: Seq[String] = options.sorted

                      if (passedOpts.contains("indexed"))
                        throw new RuntimeException(s"`indexed` option unexpectedly passed. Is presumed a default.")

                      if (passedOpts.contains("isComponent") && curTpe != "ref")
                        throw new RuntimeException(s"Can only apply `isComponent` option to ref attributes.")

                      val availableOptions    = List("noHistory", "uniqueValue", "uniqueIdentity", "isComponent", "fulltext")
                      val nonAvailableOptions = passedOpts.diff(availableOptions)

                      if (nonAvailableOptions.nonEmpty)
                        throw new RuntimeException(s"Found unrecognized option(s): " + nonAvailableOptions.mkString(", ") +
                          s"\nAvailable options: noHistory, uniqueValue, uniqueIdentity, isComponent, fulltext")

                      val (_, isComponent, noHistory, fulltext, unique) = Schema.a(curAttrFull).isComponent$.noHistory$.fulltext$.unique$.get(liveConn).head
                      val liveOpts: Seq[String]                         = Seq(
                        //                      Some("indexed"), // default
                        if (isComponent.isDefined) Some("isComponent") else None,
                        if (noHistory.isDefined) Some("noHistory") else None,
                        if (fulltext.isDefined) Some("fulltext") else None,
                        //                      unique,
                        if (unique.isDefined) {
                          unique.get match {
                            case "value"    => Some("uniqueValue")
                            case "identity" => Some("uniqueIdentity")
                          }
                        } else None
                      ).flatten.sorted

                      val obsoleteOpts: Seq[String] = curOptions.diff(passedOpts)
                      val newOpts     : Seq[String] = passedOpts.diff(curOptions)

                      //                    println("-----------------------------------")
                      //                    println("liveOpts    : " + liveOpts.mkString(", "))
                      //                    println("curOptions  : " + curOptions.mkString(", "))
                      //                    println("passedOpts  : " + passedOpts.mkString(", "))
                      //                    println("obsoleteOpts: " + obsoleteOpts.mkString(", "))
                      //                    println("newOpts     : " + newOpts.mkString(", "))

                      if (obsoleteOpts.contains("fulltext"))
                        throw new RuntimeException(s"Can't remove fulltext option from existing attribute.")

                      // Check that current client schema options are in sync with live options
                      if (curOptions != liveOpts)
                        throw new RuntimeException(
                          s"""Client schema options not in sync with live options:
                             |<br>Client options: ${curOptions.mkString(", ")}
                             |<br>Live options  : ${liveOpts.mkString(", ")}""".stripMargin)

                      // live

                      // Turn off obsolete options
                      obsoleteOpts.foreach {
                        case "uniqueValue" =>
                          liveConn.transact(Util.list(
                            Util.list(":db/retract", curAttrFull, ":db/unique", ":db.unique/value"),
                            Util.list(":db/add", ":db.part/db", ":db.alter/attribute", curAttrFull)
                          ).asInstanceOf[util.List[AnyRef]])

                          rollbackOptionsStmtsMap.add(Util.map(":db/id", curAttrFull, ":db/unique", ":db.unique/value"))


                        case "uniqueIdentity" =>
                          liveConn.transact(Util.list(
                            Util.list(":db/retract", curAttrFull, ":db/unique", ":db.unique/identity"),
                            Util.list(":db/add", ":db.part/db", ":db.alter/attribute", curAttrFull)
                          ).asInstanceOf[util.List[AnyRef]])

                          rollbackOptionsStmtsMap.add(Util.map(":db/id", curAttrFull, ":db/unique", ":db.unique/identity"))

                        case "noHistory" =>
                          liveConn.transact(Util.list(
                            Util.list(":db/retract", curAttrFull, ":db/noHistory", true.asInstanceOf[AnyRef]),
                            Util.list(":db/add", ":db.part/db", ":db.alter/attribute", curAttrFull)
                          ).asInstanceOf[util.List[AnyRef]])

                          rollbackOptionsStmtsMap.add(Util.map(":db/id", curAttrFull, s":db/noHistory", true.asInstanceOf[AnyRef]))

                        case "isComponent" =>
                          liveConn.transact(Util.list(
                            Util.map(":db/id", curAttrFull, s":db/isComponent", false.asInstanceOf[AnyRef])
                          ).asInstanceOf[util.List[AnyRef]])

                          rollbackOptionsStmtsMap.add(Util.map(":db/id", curAttrFull, s":db/isComponent", true.asInstanceOf[AnyRef]))

                        case other => throw new RuntimeException(s"Unexpected obsolete option: `$other`.")
                      }

                      // Turn on new options
                      val onStmts: Seq[jMap[_, _]] = newOpts.map {
                        case "uniqueValue" =>
                          rollbackOptionsStmtsList.add(Util.list(":db/retract", curAttrFull, ":db/unique", ":db.unique/value"))
                          rollbackOptionsStmtsList.add(Util.list(":db/add", ":db.part/db", ":db.alter/attribute", curAttrFull))
                          Util.map(":db/id", curAttrFull, ":db/unique", ":db.unique/value")

                        case "uniqueIdentity" =>
                          rollbackOptionsStmtsList.add(Util.list(":db/retract", curAttrFull, ":db/unique", ":db.unique/identity"))
                          rollbackOptionsStmtsList.add(Util.list(":db/add", ":db.part/db", ":db.alter/attribute", curAttrFull))
                          Util.map(":db/id", curAttrFull, ":db/unique", ":db.unique/identity")

                        case newOption =>
                          rollbackOptionsStmtsMap.add(Util.map(":db/id", curAttrFull, s":db/$newOption", false.asInstanceOf[AnyRef]))
                          Util.map(":db/id", curAttrFull, s":db/$newOption", true.asInstanceOf[AnyRef])
                      }
                      liveConn.transact(Util.list(onStmts: _*).asInstanceOf[util.List[AnyRef]])

                      // meta
                      meta_Attribute(attrE).options(passedOpts).update

                      // client
                      updatedAttrDef = updatedAttrDef.copy(options$ = if (passedOpts.nonEmpty) Some(passedOpts.toSet) else None)
                    }


                    // attribute doc ..............................................................................

                    if (curDoc != doc) {

                      // live
                      doc match {
                        case Some("") | None =>
                          liveConn.transact(Util.list(
                            Util.list(":db/retract", curAttrFull, ":db/doc", curDoc.get),
                            Util.list(":db/add", ":db.part/db", ":db.alter/attribute", curAttrFull)
                          ).asInstanceOf[util.List[AnyRef]])

                          rollbackDocStmtsMap.add(Util.map(":db/id", curAttrFull, ":db/doc", curDoc.get))

                        case Some(txt) =>
                          liveConn.transact(Util.list(
                            Util.map(":db/id", curAttrFull, ":db/doc", doc.get)
                          ).asInstanceOf[util.List[AnyRef]])

                          rollbackDocStmtsList.add(Util.list(":db/retract", curAttrFull, ":db/doc", doc.get))
                          rollbackDocStmtsList.add(Util.list(":db/add", ":db.part/db", ":db.alter/attribute", curAttrFull))
                      }

                      // meta
                      if (doc.isDefined) {
                        val docTxt = doc.get
                        meta_Attribute(attrE).doc(docTxt).update
                      } else
                        meta_Attribute(attrE).doc().update

                      // client
                      updatedAttrDef = updatedAttrDef.copy(doc$ = doc)
                    }


                    // attribute name .............................................................................

                    if (curAttr != newAttr) {
                      // live
                      rollbackAttrStmtsMap.add(Util.map(":db/id", s":$nsFull/$newAttr", ":db/ident", s":$nsFull/$curAttr"))
                      liveConn.transact(Util.list(
                        Util.map(":db/id", s":$nsFull/$curAttr", ":db/ident", s":$nsFull/$newAttr")
                      ).asInstanceOf[util.List[AnyRef]])

                      // meta
                      meta_Attribute(attrE).name(newAttr).update

                      // client
                      updatedAttrDef = updatedAttrDef.copy(name = newAttr)
                    }


                    // attribute group ............................................................................

                    if (curAttrGroup != attrGroup) {

                      // meta
                      if (attrGroup.isDefined) {
                        val attrGroupValue = attrGroup.get
                        meta_Attribute(attrE).attrGroup(attrGroupValue).update
                      } else {
                        meta_Attribute(attrE).attrGroup().update
                      }

                      // client
                      updatedAttrDef = updatedAttrDef.copy(attrGroup$ = attrGroup)
                    }


                    // Update client schema and def file ----------------------------------------------------------

                    // Replace updated attribute definition in client model
                    val updatedSchema: MetaSchema = MetaSchema(schema.parts.map {
                      case partDef@Part(_, `part`, _, _, nss) => {
                        val nsDefs: Seq[Ns] = nss.map {
                          case nsDef@Ns(_, `nsOnly`, _, _, _, attrs) => {
                            val attrDefs: Seq[Attr] = attrs.map {
                              case Attr(_, `curAttr`, _, _, _, _, _, _, _, _, _, _, _) => updatedAttrDef
                              case attrDef                                             => attrDef
                            }
                            nsDef.copy(attrs = attrDefs)
                          }
                          case nsDef                                 => nsDef
                        }
                        partDef.copy(nss = nsDefs)
                      }
                      case partDef                            => partDef
                    })

                    // Update def file
                    DefFile(db).recreateFrom(updatedSchema)

                  } catch {

                    // Roll-back ==================================================================================
                    case error: Throwable => try {

                      // Restore order of meta attributes
                      metaAttrs.foreach { case (e, pos1, _) => meta_Attribute(e).pos(pos1).update }

                      // Restore meta attribute data
                      meta_Attribute(attrE).pos(curPos).name(curAttr).card(curCard).tpe(curTpe).enums$(curEnums).refNs$(curRefNs).options$(curOptions0).doc$(curDoc).update

                      // Restore def file
                      DefFile(db).recreateFrom(schema0)

                      // Rollback changes to live db if any
                      if (rollbackCardStmtsList.size > 0) {
                        liveConn.transact(rollbackCardStmtsList.asInstanceOf[jList[AnyRef]])
                        liveConn.transact(rollbackCardStmtsMap.asInstanceOf[jList[AnyRef]])
                      }

                      // Only current static enum values will be available. So no need to rollback newly asserted enum values

                      if (rollbackOptionsStmtsList.size > 0) {
                        liveConn.transact(rollbackOptionsStmtsList.asInstanceOf[jList[AnyRef]])
                        liveConn.transact(rollbackOptionsStmtsMap.asInstanceOf[jList[AnyRef]])
                      }

                      if (rollbackDocStmtsList.size > 0) {
                        liveConn.transact(rollbackDocStmtsList.asInstanceOf[jList[AnyRef]])
                        liveConn.transact(rollbackDocStmtsMap.asInstanceOf[jList[AnyRef]])
                      }

                      if (rollbackAttrStmtsMap.size > 0) {
                        liveConn.transact(rollbackAttrStmtsMap.asInstanceOf[jList[AnyRef]])
                      }

                      Left("Successfully rolled back from error: " + error.getMessage)
                    } catch {
                      case rollbackError: Throwable => Left("Unsuccessfully tried to roll back from error! " +
                        "Please check meta db that might be in an invalid state. Unexpected rollback error: " + rollbackError.getMessage)
                    }
                  }
                }

                case Nil       => Left(s"Couldn't find attribute `$curAttr` in namespace `$nsOnly` in partition `$part` in database `$db`.")
                case moreAttrs => Left(s"Unexpectedly found ${moreAttrs.size} attributes named `$curAttr` in namespace `$nsOnly` in partition `$part` in database `$db`.")
              }

              case 0       => Left(s"Couldn't find namespace `$nsOnly` in partition `$part` in database `$db`.")
              case nsCount => Left(s"Unexpectedly found $nsCount namespaces named `$nsOnly` in partition `$part` in database `$db`.")
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


  def delete(schema: MetaSchema, db: String, part: String, nsOnly: String, attr: String): Either[String, MetaSchema] = {
    if (db.isEmpty) {
      Left("Empty db name.")
    } else if (part.isEmpty) {
      Left("Empty partition name.")
    } else if (nsOnly.isEmpty) {
      Left(s"Empty namespace name.")
    } else if (attr.isEmpty) {
      Left(s"Empty attribute name.")
    } else {
      implicit val metaConn = Conn(base + "/meta")
      withTransactor {
        meta_Db.e.name_(db).get.size match {
          case 1 => meta_Db.name_(db).Partitions.e.name_(part).get.size match {
            case 1 => meta_Db.name_(db).Partitions.name_(part).Namespaces.e.name_(nsOnly).get.size match {
              case 1 => meta_Db.name_(db).Partitions.name_(part).Namespaces.name_(nsOnly).Attrs.e.pos.name_(attr).card.tpe.enums$.refNs$.options$.doc$.entityCount$.get match {

                // Current attribute meta data
                case Seq((attrE, pos, card, tpe, optEnums, optRefNs, optOptions, optDoc, optEntityCount)) =>
                  val liveConn  = Conn(base + "/" + db)
                  val metaAttrs = meta_Db.name_(db).Partitions.name_(part).Namespaces.name_(nsOnly).Attrs.e.pos.>(pos).get

                  val rollbackAttrStmtsMap = new java.util.ArrayList[jMap[_, _]]()

                  try {
                    val fullNs   = getFullNs(part, nsOnly)
                    val fullAttr = getFullAttr(part, nsOnly, attr)

                    // How many entities have this attribute asserted?
                    val entityCount = getEntityCount(liveConn, fullAttr)
                    if (entityCount != 0) {
                      optEntityCount match {
                        case Some(savedEntityCount) if savedEntityCount != entityCount =>
                          // Update out-of-sync entity/value count while we're here anyway (ignoring topValues)
                          val distinctValueCount = getDistinctValueCount(liveConn, fullAttr)
                          meta_Attribute(attrE).entityCount(entityCount).distinctValueCount(distinctValueCount).topValues().update
                        case _                                                         => // no action, entity count is correct/unset
                      }
                      Left(s"Can't delete attribute `$attr` asserted with $entityCount entities. Please retract or transfer attribute value(s) first.")

                    } else {
                      // No _current_ entities have this attribute asserted

                      // live - "park" obsolete attribute name
                      liveConn.transact(Util.list(
                        Util.map(":db/id", s":$fullNs/$attr", ":db/ident", s":-$fullNs/$attr")
                      ).asInstanceOf[util.List[AnyRef]])

                      rollbackAttrStmtsMap.add(
                        Util.map(":db/id", s":-$fullNs/$attr", ":db/ident", s":$fullNs/$attr")
                      )

                      // Retract meta attribute
                      attrE.retract

                      // Shift attribute positions after deleted attribute
                      metaAttrs.foreach { case (e, o) =>
                        val decrPos = o - 1
                        meta_Attribute(e).pos(decrPos).update
                      }

                      // client
                      val updatedPartitions = schema.parts.map {
                        case p if p.name == part => {
                          val updatedNss = p.nss.map {
                            case ns1 if ns1.name == nsOnly => {
                              val updatedAttrs = ns1.attrs.flatMap {
                                case a if a.pos == pos => Nil
                                case a if a.pos > pos  => Seq(a.copy(pos = a.pos - 1))
                                case a                 => Seq(a)
                              }
                              ns1.copy(attrs = updatedAttrs)
                            }
                            case ns1                       => ns1
                          }
                          p.copy(nss = updatedNss)
                        }
                        case p                   => p
                      }

                      // def file
                      DefFile(db).recreateFrom(MetaSchema(updatedPartitions))
                    }

                  } catch {

                    // Roll-back ==================================================================================
                    case error: Throwable => try {

                      // Rollback meta positions for shifted attributes in namespace
                      metaAttrs.foreach { case (e, p) => meta_Attribute(e).pos(p).update }

                      // Recreate deleted meta attribute
                      meta_Attribute(attrE).pos(pos).name(attr).card(card).tpe(tpe).enums$(optEnums).refNs$(optRefNs).options$(optOptions).doc$(optDoc)
                        .entityCount$(optEntityCount).update

                      // Rollback def file
                      DefFile(db).recreateFrom(schema)

                      // "Un-park" live attribute name
                      if (rollbackAttrStmtsMap.size > 0) {
                        liveConn.transact(rollbackAttrStmtsMap.asInstanceOf[jList[AnyRef]])
                      }

                      Left("Successfully rolled back from error: " + error.getMessage)
                    } catch {
                      case rollbackError: Throwable => Left("Unsuccessfully tried to roll back from error! " +
                        "Please check meta db that might be in an invalid state. Unexpected rollback error: " + rollbackError.getMessage)
                    }
                  }

                case Nil       => Left(s"Couldn't find attribute `$attr` in namespace `$nsOnly` in partition `$part` in database `$db`.")
                case moreAttrs => Left(s"Unexpectedly found ${moreAttrs.size} attributes named `$attr` in namespace `$nsOnly` in partition `$part` in database `$db`.")
              }

              case 0       => Left(s"Couldn't find namespace `$nsOnly` in partition `$part` in database `$db`.")
              case nsCount => Left(s"Unexpectedly found $nsCount namespaces named `$nsOnly` in partition `$part` in database `$db`.")
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


  private def addAttrToLiveSchema(dbConn: Conn, part: String, nsOnly: String, attr: String, card: Int, tpe: String, enums: Seq[String], options: Seq[String], doc: Option[String]): Unit = {
    val base    : Seq[Object]       = Seq(
      ":db/ident", getFullAttr(part, nsOnly, attr),
      ":db/valueType", s":db.type/${tpeMoleculeDatomic(tpe)}",
      ":db/cardinality", s":db.cardinality/${cardIntStr(card)}",
      ":db/index", true.asInstanceOf[Object]
    )
    val opts                        = options.filterNot(_ == "indexed").flatMap {
      case "uniqueValue"    => Seq(s":db/unique", ":db.unique/value")
      case "uniqueIdentity" => Seq(s":db/unique", ":db.unique/identity")
      case opt              => Seq(s":db/$opt", true.asInstanceOf[Object])
    }
    val doc1                        = if (doc.isEmpty) Nil else Seq(":db/doc", doc.get)
    val attrDef : jMap[_, _]        = Util.map(base ++ opts ++ doc1: _*)
    val enumDefs: Seq[jMap[_, _]]   = if (enums.isEmpty) Nil else {
      val partition = if (part.isEmpty) ":db.part/user" else s":$part"
      val enumNs    = getFullNs(part, nsOnly)
      enums.map { enum =>
        Util.map(":db/id", Peer.tempid(partition), ":db/ident", s":$enumNs.$attr/$enum")
      }
    }
    val stmts   : util.List[AnyRef] = (attrDef +: enumDefs).asJava.asInstanceOf[util.List[AnyRef]]
    dbConn.transact(stmts)
  }
}