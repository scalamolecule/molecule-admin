package moleculeadmin.server

import java.lang.{Long => jLong}
import java.net.URI
import java.util.{Date, UUID}
import datomic.{Peer, Util}
import db.admin.dsl.meta._
import db.core.dsl.coreTest.Ns
import molecule.api.Entity
import molecule.api.out10._
import molecule.ast.model.{Atom, Model, NoValue}
import molecule.ast.transactionModel.{Add, Retract, RetractEntity}
import molecule.facade.{Conn, TxReport}
import molecule.transform.Model2Transaction
import moleculeadmin.server.query.Rows2QueryResult
import moleculeadmin.shared.api.QueryApi
import moleculeadmin.shared.ast.query.{Col, QueryDTO, QueryResult}
import moleculeadmin.shared.ops.transform.Molecule2Model
import scala.collection.JavaConverters._


class QueryBackend extends QueryApi with Base {

  // Todo: this works but seems like a hack that would be nice to avoid although the impact of
  // a few input variables is negible.
  // To avoid type combination explosions from multiple inputs of various types to be transferred
  // with autowire/boopickle, we cast all input variable values as String on the client and then
  // cast them back to their original type here and pass them as Object's to Datomic.
  def cast(pair: (String, String)): Object = pair match {
    case ("String", v)     => v.asInstanceOf[Object]
    case ("Int", v)        => v.toInt.asInstanceOf[Object]
    case ("Long", v)       => v.toLong.asInstanceOf[Object]
    case ("Float", v)      => v.toDouble.asInstanceOf[Object]
    case ("Double", v)     => v.toDouble.asInstanceOf[Object]
    case ("BigInt", v)     => BigInt.apply(v).asInstanceOf[Object]
    case ("BigDecimal", v) => BigDecimal(v).asInstanceOf[Object]
    case ("Boolean", v)    => v.toBoolean.asInstanceOf[Object]
    case ("Date", v)       => str2date(v).asInstanceOf[Object]
    case ("UUID", v)       => java.util.UUID.fromString(v).asInstanceOf[Object]
    case ("URI", v)        => new java.net.URI(v).asInstanceOf[Object]
    case _                 => sys.error("Unexpected input pair to cast")
  }

  def inputs(lists: Seq[(Int, AnyRef)]): Seq[Object] = lists.sortBy(_._1).map(_._2).map {
    case l: Seq[_]                   => Util.list(l.map {
      case l2: Seq[_]                  => Util.list(l2.map(v => cast(v.asInstanceOf[(String, String)])): _*)
      case pair@(_: String, _: String) => cast(pair.asInstanceOf[(String, String)])
      case _                           => sys.error("Unexpected input values")
    }: _*)
    case pair@(_: String, _: String) => cast(pair.asInstanceOf[(String, String)])
    case _                           => sys.error("Unexpected input values")
  }


  override def query(
    db: String,
    datalogQuery: String,
    rules: Option[String],
    l: Seq[(Int, (String, String))],
    ll: Seq[(Int, Seq[(String, String)])],
    lll: Seq[(Int, Seq[Seq[(String, String)]])],
    maxRows: Int,
    cols: Seq[Col]
  ): Either[Seq[String], QueryResult] = try {

    val conn      = Conn(base + "/" + db)
    val allInputs = if (rules.isEmpty)
      conn.db +: inputs(l ++ ll ++ lll)
    else
      conn.db +: rules.get +: inputs(l ++ ll ++ lll)

    val t0        = System.currentTimeMillis
    val allRows   = Peer.q(datalogQuery, allInputs: _*)
    val queryTime = System.currentTimeMillis - t0
    //    t.log(1, "Query")

    val rowCountAll = allRows.size
    val rowCount    = if (maxRows == -1 || rowCountAll < maxRows) rowCountAll else maxRows

    println("--------------------")
    println(datalogQuery)
    println("rowCountAll: " + rowCountAll)
    println("maxRows    : " + (if (maxRows == -1) "all" else maxRows))
    println("rowCount   : " + rowCount)
    //    println("cols       : " + cols)
    allRows.asScala.take(10) foreach println

    if (rowCount == 0)
      Left(Nil)
    else {
      val queryResult = Rows2QueryResult(
        allRows, rowCountAll, rowCount, cols, queryTime).get
      //      t.log(2, "To QueryResult")
      Right(queryResult)
    }
  } catch {
    case t: Throwable => Left(t.getMessage +: t.getStackTrace.toSeq.map(_.toString))
  }

  override def touchEntity(db: String, eid: Long): List[(String, String)] = {
    val conn = Conn(base + "/" + db)
    Entity(conn.db.entity(eid), conn, eid.asInstanceOf[Object]).touchListMax(1)
      .map {
        case (a, date: Date) => (a, date2str(date))
        case (a, vs: Seq[_]) =>
          if (vs.nonEmpty && vs.head.isInstanceOf[Date])
            (a, vs.map(v =>
              date2str(v.asInstanceOf[Date])
            ).mkString("__~~__"))
          else
            (a, vs.mkString("__~~__"))
        case (a, v)          => (a, v.toString)
      }
  }

  def formatEmpty(v: Any): String = {
    val s = v.toString
    if (s.trim.isEmpty) s"{$s}" else s
  }

  private def ident(conn: Conn, e: jLong): String =
    conn.db.entity(e).get(":db/ident").toString

  override def getTxData(
    db: String,
    tx: Long,
    enumAttrs: Seq[String]
  ): Array[(Long, String, String, Boolean)] = {
    val conn   = Conn(base + "/" + db)
    val result = datomic.Peer.q(
      """[:find ?e ?aStr ?v ?op
        |:in $ ?log ?tx
        |:where [(tx-data ?log ?tx) [[?e ?a ?v _ ?op]]]
        |       [?a :db/ident ?idIdent]
        |       [(str ?idIdent) ?aStr]
        |]""".stripMargin,
      conn.datomicConn.db(),
      conn.datomicConn.log(),
      tx.asInstanceOf[Object]
    )
    val it     = result.iterator()
    val rows   = new Array[(Long, String, String, Boolean)](result.size)
    var attr   = ""
    var i      = 0
    while (it.hasNext) {
      val row = it.next()
      attr = row.get(1).asInstanceOf[String]
      rows(i) = (
        row.get(0).asInstanceOf[Long],
        attr,
        row.get(2) match {
          case s: String                            => formatEmpty(s)
          case e: jLong if enumAttrs.contains(attr) => ident(conn, e)
          case d: Date                              => date2str(d)
          case v                                    => formatEmpty(v)
        },
        row.get(3).asInstanceOf[Boolean]
      )
      i += 1
    }
    rows.sortBy(t => (t._1, t._2, t._4, t._3))
  }

  override def getEntityHistory(
    db: String,
    eid: Long,
    enumAttrs: Seq[String]
  )
  : List[(Long, Long, String, Boolean, String, String)] = {
    implicit val conn = Conn(base + "/" + db)
    Ns(eid).t.tx.txInstant.op.a.v.getHistory.map {
      case (t, tx, txInstant, op, attr, v) =>
        (t,
          tx,
          date2str(txInstant),
          op,
          attr,
          v match {
            case s: String                            => formatEmpty(s)
            case e: jLong if enumAttrs.contains(attr) => ident(conn, e)
            case d: Date                              => date2str(d)
            case v                                    => formatEmpty(v)
          }
        )
    }
  }

  override def getTFromTx(tx: Long): Long = Peer.toT(tx)

  override def getTxFromT(t: Long): Long = Peer.toTx(t).asInstanceOf[Long]

  override def getTTxFromTxInstant(db: String, txInstantStr: String): (Long, Long) = {
    val rawConn   = Conn(base + "/" + db).datomicConn
    val txInstant = str2date(txInstantStr)
    val result    = datomic.Peer.q(
      """[:find ?t ?tx
        |:in $ ?log ?tx
        |:where [?tx :db/txInstant ?txInstant]
        |       [(datomic.Peer/toT ^Long ?tx) ?t]
        |]""".stripMargin,
      rawConn.db(),
      rawConn.log(),
      txInstant.asInstanceOf[Object]
    )
    val row       = result.iterator().next()
    (row.get(0).asInstanceOf[Long], row.get(1).asInstanceOf[Long])
  }

  def withTransactor[T](
    body: => Either[String, T]
  )(implicit conn: Conn): Either[String, T] = try {
    // Check if transactor responds by sending a Future back
    conn.datomicConn.sync()
    // Execute body of work
    body
  } catch {
    case _: Throwable => Left(
      "Datomic Transactor unavailable. Please restart it and try the operation again.")
  }


  override def upsertQuery(db: String, query: QueryDTO): Either[String, String] = {
    implicit val conn = Conn(base + "/meta")
    withTransactor {
      try {
        // Use admin for now
        val userId = user_User.e.username_("admin").get match {
          case List(eid) => eid
          case Nil       => user_User.username("admin").save.eid
        }
        val dbId   = meta_Db.e.name_(db).get.headOption match {
          case Some(eid) => eid
          case None      =>
            throw new RuntimeException(
              s"Unexpectedly couldn't find database name `$db` in meta database.")
        }

        // One DbSettings per db for now
        val dbSettingsId = user_User(userId).DbSettings.e.db_(dbId).get match {
          case List(eid) => eid
          case Nil       =>
            val dbSettingsId1 = user_DbSettings.db(dbId).save.eid
            user_User(userId).dbSettings.assert(dbSettingsId1).update
            dbSettingsId1
        }

        val QueryDTO(molecule1, _, _,
        isFavorite, showGrouped, groupedCols, colSettings) = query

        user_DbSettings(dbSettingsId).Queries.e.molecule_(molecule1).get match {
          case Nil =>
            val newQueryId =
              user_Query.molecule.part.ns.isFavorite.showGrouped.groupedCols.ColSettings.*(
                user_ColSetting.colIndex.sortDir.sortPos
              ).insert(List(QueryDTO.unapply(query).get)).eid

            user_DbSettings(dbSettingsId).queries.assert(newQueryId).update
            Right("Successfully inserted query")

          case Seq(queryId) =>
            // Re-insert col settings
            user_Query(queryId).colSettings().update
            val colSettingIds =
              user_ColSetting.colIndex.sortDir.sortPos.insert(colSettings).eidSet

            user_Query(queryId)
              .isFavorite(isFavorite)
              .showGrouped(showGrouped)
              .groupedCols(groupedCols)
              .colSettings(colSettingIds)
              .update

            Right("Successfully updated query")

          case queryIds =>
            Left(s"`$molecule1` unexpectedly found ${queryIds.length} times.")
        }
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }

  override def updateQuery(db: String, query: QueryDTO): Either[String, String] = {
    implicit val conn = Conn(base + "/meta")
    withTransactor {
      try {
        // Use admin for now
        val userId = user_User.e.username_("admin").get match {
          case List(eid) => eid
          case Nil       => user_User.username("admin").save.eid
        }
        val dbId   = meta_Db.e.name_(db).get.headOption match {
          case Some(eid) => eid
          case None      =>
            throw new RuntimeException(
              s"Unexpectedly couldn't find database name `$db` in meta database.")
        }

        // One DbSettings per db for now
        val dbSettingsId = user_User(userId).DbSettings.e.db_(dbId).get match {
          case List(eid) => eid
          case Nil       =>
            val dbSettingsId1 = user_DbSettings.db(dbId).save.eid
            user_User(userId).dbSettings.assert(dbSettingsId1).update
            dbSettingsId1
        }

        val QueryDTO(molecule1, _, _,
        isFavorite, showGrouped, groupedCols, colSettings) = query

        val queryIds = user_DbSettings(dbSettingsId).Queries.e.molecule_(molecule1).get

        if (queryIds.isEmpty) {
          Left(s"`$molecule1` unexpectedly not found.")
        } else if (queryIds.length > 1) {
          Left(s"`$molecule1` unexpectedly found ${queryIds.length} times.")
        } else {
          val queryId = queryIds.head
          // Re-insert col settings
          user_Query(queryId).colSettings().update
          val colSettingIds =
            user_ColSetting.colIndex.sortDir.sortPos.insert(colSettings).eidSet
          user_Query(queryId)
            .isFavorite(isFavorite)
            .showGrouped(showGrouped)
            .groupedCols(groupedCols)
            .colSettings(colSettingIds)
            .update

          Right("ok")
        }
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }

  override def retractQuery(db: String, query: QueryDTO): Either[String, String] = {
    implicit val conn = Conn(base + "/meta")
    val molecule1 = query.molecule
    withTransactor {
      try {
        user_User.username_("admin")
          .DbSettings.Db.name_(db)
          ._DbSettings.Queries.e.molecule_(molecule1)
          .get match {
          case Nil           => Left(s"Unexpectedly couldn't find saved molecule `$molecule1` in meta database.")
          case List(queryId) =>
            queryId.retract
            Right("ok")
          case favIds        =>
            Left(s"Unexpectedly found ${favIds.size} instances of saved molecule `$molecule1` in meta database.")
        }
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }

  override def toggleMarker(
    db: String,
    dbSettingsIdOpt: Option[Long],
    tpe: String,
    eid: Long,
    isOn: Boolean
  ): Either[String, Long] = {
    implicit val conn = Conn(base + "/meta")
    withTransactor {
      try {
        val dbSettingsId = dbSettingsIdOpt.getOrElse {
          val userId = user_User.e.username_("admin").get match {
            case List(eid) => eid
            case Nil       => user_User.username("admin").save.eid
          }
          user_User(userId).DbSettings.e.Db.name_(db).get match {
            case List(dbSettingsId) => dbSettingsId
            case Nil                =>
              val dbId = meta_Db.e.name_(db).get.head
              user_DbSettings.db(dbId).save.eid
          }
        }
        if (isOn) {
          tpe match {
            case "star"  => user_DbSettings(dbSettingsId).stars.retract(eid).update
            case "flag"  => user_DbSettings(dbSettingsId).flags.retract(eid).update
            case "check" => user_DbSettings(dbSettingsId).checks.retract(eid).update
          }
        } else {
          tpe match {
            case "star"  => user_DbSettings(dbSettingsId).stars.assert(eid).update
            case "flag"  => user_DbSettings(dbSettingsId).flags.assert(eid).update
            case "check" => user_DbSettings(dbSettingsId).checks.assert(eid).update
          }
        }
        Right(dbSettingsId)
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }

  override def setMarkers(
    db: String,
    dbSettingsIdOpt: Option[Long],
    tpe: String,
    eids: Set[Long],
    newState: Boolean
  ): Either[String, Long] = {
    implicit val conn = Conn(base + "/meta")
    withTransactor {
      try {
        val dbSettingsId = dbSettingsIdOpt.getOrElse {
          val userId = user_User.e.username_("admin").get match {
            case List(eid) => eid
            case Nil       => user_User.username("admin").save.eid
          }
          user_User(userId).DbSettings.e.Db.name_(db).get match {
            case List(dbSettingsId) => dbSettingsId
            case Nil                =>
              val dbId = meta_Db.e.name_(db).get.head
              user_DbSettings.db(dbId).save.eid
          }
        }
        if (newState) {
          tpe match {
            case "star"  => user_DbSettings(dbSettingsId).stars.assert(eids).update
            case "flag"  => user_DbSettings(dbSettingsId).flags.assert(eids).update
            case "check" => user_DbSettings(dbSettingsId).checks.assert(eids).update
          }
        } else {
          tpe match {
            case "star"  => user_DbSettings(dbSettingsId).stars.retract(eids).update
            case "flag"  => user_DbSettings(dbSettingsId).flags.retract(eids).update
            case "check" => user_DbSettings(dbSettingsId).checks.retract(eids).update
          }
        }
        Right(dbSettingsId)
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }

  override def unmarkAll(
    db: String,
    dbSettingsIdOpt: Option[Long],
    tpe: String,
  ): Either[String, Long] = {
    implicit val conn = Conn(base + "/meta")
    withTransactor {
      try {
        val dbSettingsId = dbSettingsIdOpt.getOrElse {
          val userId = user_User.e.username_("admin").get match {
            case List(eid) => eid
            case Nil       => user_User.username("admin").save.eid
          }
          user_User(userId).DbSettings.e.Db.name_(db).get match {
            case List(dbSettingsId) => dbSettingsId
            case Nil                =>
              val dbId = meta_Db.e.name_(db).get.head
              user_DbSettings.db(dbId).save.eid
          }
        }

        tpe match {
          case "star"  => user_DbSettings(dbSettingsId).stars().update
          case "flag"  => user_DbSettings(dbSettingsId).flags().update
          case "check" => user_DbSettings(dbSettingsId).checks().update
        }
        Right(dbSettingsId)
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }

  override def saveSettings(pairs: Seq[(String, String)]): Either[String, String] = {
    implicit val conn = Conn(base + "/meta")
    withTransactor {
      try {
        // Use admin for now
        val userId = user_User.e.username_("admin").get match {
          case List(eid) => eid
          case Nil       => user_User.username("admin").save.eid
        }
        // Save key/value settings
        user_User(userId).settings.assert(pairs).update
        Right("ok")
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }

  def getStrCaster(tpe: String, enumPrefix: String): String => Any = {
    tpe match {
      case "String"     => (v: String) => enumPrefix + v
      case "Boolean"    => (v: String) => v.toBoolean
      case "Date"       => (v: String) => str2date(v)
      case "UUID"       => (v: String) => UUID.fromString(v)
      case "URI"        => (v: String) => new URI(v)
      case "BigInt"     => (v: String) => BigInt(v)
      case "BigDecimal" => (v: String) => BigDecimal(v)
    }
  }

  def getNumCaster(tpe: String): Double => Any = {
    tpe match {
      case "Int" | "Long" | "ref" => (v: Double) => v.toLong
      case "Float" | "Double"     => (v: Double) => v
    }
  }

  def update[T](
    db: String,
    attrFull: String,
    data: Seq[(Long, Seq[T], Seq[T])],
    cast: T => Any,
  ): Either[String, (Long, Long, String)] = {
    implicit val conn = Conn(base + "/" + db)
    val stmtss = data.map {
      case (eid, retracts, asserts) =>
        retracts.map(v => Retract(eid, attrFull, cast(v), NoValue)) ++
          asserts.map(v => Add(eid, attrFull, cast(v), NoValue))
    }
    withTransactor {
      try {
        val txR: TxReport = conn.transact(stmtss)
        Right((txR.t, txR.tx, date2str(txR.inst)))
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }

  override def updateStr(
    db: String,
    attrFull: String,
    attrType: String,
    enumPrefix: String,
    data: Seq[(Long, Seq[String], Seq[String])]
  ): Either[String, (Long, Long, String)] = {
    update(db, attrFull, data, getStrCaster(attrType, enumPrefix))
  }


  override def updateNum(
    db: String,
    attrFull: String,
    attrType: String,
    data: Seq[(Long, Seq[Double], Seq[Double])],
  ): Either[String, (Long, Long, String)] = {
    update(db, attrFull, data, getNumCaster(attrType))
  }

  import moleculeadmin.shared.ast.schema

  override def insert(
    db: String,
    molecule: String,
    nsMap: Map[String, schema.Ns],
    rawData: Seq[String]
  ): Either[String, Long] = {
    // Model without initial entity id
    val elements = new Molecule2Model(molecule, nsMap).getModel.right.get.tail
    implicit val conn = Conn(base + "/" + db)
    val data = elements.zipWithIndex.map {
      case (a: Atom, i) =>
        val v = rawData(i)
        a.tpe match {
          case "String"           => a.enumPrefix.getOrElse("") + v
          case "Int" | "Long"     => v.toLong
          case "Float" | "Double" => v.toDouble
          case "Boolean"          => v.toBoolean
          case "Date"             => str2date(v)
          case "UUID"             => UUID.fromString(v)
          case "URI"              => new URI(v)
          case "BigInt"           => BigInt(v)
          case "BigDecimal"       => BigDecimal(v)
        }
      case other =>
        throw new IllegalArgumentException("Unexpected Model Element: " + other)
    }
    val stmtss = Model2Transaction(conn, Model(elements)).insertStmts(Seq(data))
    withTransactor {
      try {
        Right(conn.transact(stmtss).eid)
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }

  override def retractEntity(db: String, eid: Long): Either[String, Long] = {
    implicit val conn = Conn(base + "/" + db)
    val stmtss = Seq(Seq(RetractEntity(eid)))
    println("retractEntity: " + stmtss)
    withTransactor {
      try {
        val txR: TxReport = conn.transact(stmtss)
        Right(txR.tx)
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }
  }
}
