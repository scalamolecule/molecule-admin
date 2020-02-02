package moleculeadmin.server

import java.lang.{Long => jLong}
import java.net.URI
import java.util.{Date, UUID, List => jList}
import datomic.{Datom, Peer, Util}
import db.admin.dsl.moleculeAdmin._
import db.core.dsl.coreTest.Ns
import molecule.api.Entity
import molecule.api.out10._
import molecule.ast.model.{Atom, Bond, Model, NoValue}
import molecule.ast.transactionModel.{Add, Retract, RetractEntity, Statement}
import molecule.facade.{Conn, TxReport}
import molecule.transform.Model2Transaction
import moleculeadmin.server.query.{ToggleBackend, Rows2QueryResult}
import moleculeadmin.shared.ast.query.{Col, QueryDTO, QueryResult}
import moleculeadmin.shared.ops.transform.Molecule2Model
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.util.Try


class QueryBackend extends ToggleBackend {

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

    val t         = Timer("Query")
    val t0        = System.currentTimeMillis
    val allRows   = Peer.q(datalogQuery, allInputs: _*)
    val queryTime = System.currentTimeMillis - t0
    t.log(1)

    val rowCountAll = allRows.size
    val rowCount    = if (maxRows == -1 || rowCountAll < maxRows) rowCountAll else maxRows

    println("--------------------")
    println(datalogQuery)
    println("rowCountAll: " + rowCountAll +
      " (query time, all rows: " + thousands(queryTime) + " ms)")
    println("maxRows    : " + (if (maxRows == -1) "all" else maxRows))
    println("rowCount   : " + rowCount)
    //    println("cols       : " + cols)
    allRows.asScala.take(10) foreach println
    t.log(2)

    if (rowCount == 0)
      Left(Nil)
    else {
      val queryResult = Rows2QueryResult(
        allRows, rowCountAll, rowCount, cols, queryTime).get
      t.log(3)
      Right(queryResult)
    }
  } catch {
    case t: Throwable => Left(t.getMessage +: t.getStackTrace.toSeq.map(_.toString))
  }

  override def touchEntity(db: String, eid: Long): List[(String, String)] = {
    val conn = Conn(base + "/" + db)
    Entity(conn.db.entity(eid), conn, eid.asInstanceOf[Object]).touchListMax(1)
      .map {
        case (a, date: Date) => (a, date2strLocal(date))
        case (a, vs: Seq[_]) =>
          if (vs.nonEmpty && vs.head.isInstanceOf[Date])
            (a, vs.map(v =>
              date2strLocal(v.asInstanceOf[Date])
            ).mkString("__~~__"))
          else
            (a, vs.mkString("__~~__"))
        case (a, v)          => (a, v.toString)
      }
  }

  private def formatEmpty(v: Any): String = {
    val s = v.toString
    if (s.trim.isEmpty) s"{$s}" else s
  }

  private def ident(conn: Conn, e: jLong): String =
    conn.db.entity(e).get(":db/ident").toString

  private def formatValue(
    conn: Conn,
    attr: String,
    v: Any,
    enumAttrs: Seq[String]
  ): String = v match {
    case s: String                            => formatEmpty(s)
    case e: jLong if enumAttrs.contains(attr) => ident(conn, e)
    case d: Date                              => date2strLocal(d)
    case v                                    => formatEmpty(v)
  }


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
        formatValue(conn, attr, row.get(2), enumAttrs),
        row.get(3).asInstanceOf[Boolean]
      )
      i += 1
    }
    rows.sortBy(t => (t._1, t._2, t._4, t._3))
  }


  override def getLastTxs(
    db: String,
    prevFirstT0: Long,
    enumAttrs: Seq[String]
  ): Either[String, Array[TxData]] = {
    val conn         = Conn(base + "/" + db)
    val datomicDb    = conn.db
    val prevFirstT   = if (prevFirstT0 == 0L) datomicDb.basisT() else prevFirstT0
    val firstT       = if (prevFirstT > 1100) prevFirstT0 - 1000 else 1001
    //    val firstT       = 1001
    val txs          = conn.datomicConn.log.txRange(firstT, null).asScala
    val txData       = new Array[TxData](txs.size)
    var txIndex      = 0
    var metaE        = 0L
    var t            = 0L
    var tx           = 0L
    var txInstantStr = ""
    var e            = 0L
    var a            = ""
    var v            = ""
    var eStrs        = new ListBuffer[String]
    var metaRefDatom = null: DatomTuple
    var refDatom     = null: DatomTuple
    var metaRefE     = 0L
    var refE         = 0L

    withTransactor {
      try {
        txs.foreach { txMap =>
          eStrs.clear()
          metaRefE = 0L
          refE = 0L
          metaRefDatom = null: DatomTuple
          refDatom = null: DatomTuple

          t = txMap.get(datomic.Log.T).asInstanceOf[Long]
          val datoms0: Seq[DatomTuple] =
            txMap.get(datomic.Log.DATA)
              .asInstanceOf[jList[Datom]].asScala
              .toList.flatMap { d =>
              e = d.e.asInstanceOf[Long]
              a = datomicDb.ident(d.a).toString
              v = formatValue(conn, a, d.v, enumAttrs)
              //              println(s"$e   $a   $v")
              if (t == 1000) {
                None
              } else if (a == ":db/txInstant") {
                tx = e
                txInstantStr = v
                eStrs += tx.toString
                metaE = tx
                // Skip txInstant datom to isolate remaining tx meta data
                None
              } else if (a.startsWith(":db.") || a.startsWith(":db/")) {
                None
              } else {
                eStrs += e.toString
                Some((e, a, v, d.added))
              }
            }.sortBy(d => (d._1, d._2, d._4, d._3))

          //          println(t)
          //          datoms0 foreach println
          //          println("------")

          val datomCount   = datoms0.length
          val txMetaDatoms = new ListBuffer[DatomTuple]
          val datoms       = new ListBuffer[DatomTuple]

          if (datomCount > 0) {
            datoms0.foreach {

              // Meta datoms

              case datom@(e, _, v, _) if e == metaE && eStrs.contains(v) =>
                metaRefDatom = datom
                metaRefE = v.toLong

              case datom@(e, _, v, _) if e == metaRefE && eStrs.contains(v) =>
                if (metaRefDatom != null)
                  txMetaDatoms += metaRefDatom
                metaRefDatom = datom
                metaE = e
                metaRefE = v.toLong

              case datom@(e, _, _, _) if e == metaRefE =>
                txMetaDatoms += metaRefDatom
                txMetaDatoms += datom
                metaRefE = 0L
                metaE = e

              case datom@(e, _, _, _) if e == metaE =>
                txMetaDatoms += datom


              // Datoms

              case datom@(_, _, v, _) if eStrs.contains(v) =>
                refDatom = datom
                refE = v.toLong

              case datom@(e, _, v, _) if e == refE && eStrs.contains(v) =>
                if (refDatom != null)
                  datoms += refDatom
                refDatom = datom
                refE = v.toLong

              case datom@(e, _, _, _) if e == refE =>
                datoms += refDatom
                datoms += datom
                refE = 0L

              case datom =>
                datoms += datom
            }

            //            txMetaDatoms foreach println
            //            println("---")
            //            datoms foreach println
            //            println("=========================")

            //            // Check split
            //            if ((txMetaDatoms ++ datoms).sortBy(d => (d._1, d._2, d._4, d._3)) !=
            //              datoms0.sortBy(d => (d._1, d._2, d._4, d._3)))
            //              throw new RuntimeException(
            //                "Unexpected divergence between datoms0 and txMetaDatoms/datoms in QueryBackend.getLastTxs:" +
            //                  "\ndatoms0:\n  " + datoms0.mkString(",\n  ") +
            //                  "\n-------------" +
            //                  "\ntxMetaDatoms:\n  " + txMetaDatoms.mkString(",\n  ") +
            //                  "\ndatoms:\n  " + datoms.mkString(",\n  ")
            //              )

            txData(txIndex) = (t, tx, txInstantStr, txMetaDatoms, datoms)
            txIndex += 1
          }
        }
        // Return data transactions (without schema txs)
        val txData1 = new Array[TxData](txIndex)
        System.arraycopy(txData, 0, txData1, 0, txIndex)
        Right(txData1)
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }(conn)
  }

  override def undoTxs(
    db: String,
    ts: Seq[Long],
    enumAttrs: Seq[String]
  ): Either[String, Array[TxData]] = {
    val conn      = Conn(base + "/" + db)
    val txMaps    =
      if (ts.length == 1)
        conn.datomicConn.log.txRange(ts.head, ts.head + 1).asScala.toList
      else
        conn.datomicConn.log.txRange(ts.head, null).asScala.toList
          .filter(txMap =>
            ts.contains(txMap.get(datomic.Log.T).asInstanceOf[Long]))
          .reverse // Undo transactions backwards
    val newTxs    = new Array[TxData](txMaps.size)
    val datoms    = new ListBuffer[DatomTuple]
    val undoneTs  = new ListBuffer[Long]
    var txIndex   = 0
    var e         = 0L
    var a         = ""
    var v         = ""
    var prevValue = ""
    withTransactor {
      try {
        txMaps.foreach { txMap =>
          val undoneT   = txMap.get(datomic.Log.T).asInstanceOf[Long]
          val tx        = Peer.toTx(undoneT).asInstanceOf[Long]
          val rawDatoms = txMap.get(datomic.Log.DATA)
            .asInstanceOf[jList[Datom]].asScala.distinct

          datoms.clear()
          val stmts                 = rawDatoms.flatMap { d =>
            e = d.e.asInstanceOf[Long]
            a = conn.db.ident(d.a).toString
            v = formatValue(conn, a, d.v, enumAttrs)
            val stmt: Option[Statement] =
              if (a == ":db/txInstant" ||
                e == tx ||
                Try(prevValue.toLong).isSuccess && e == prevValue.toLong) {
                // metadata - todo: do we catch all this way?
                None
              } else {
                datoms += ((e, a, v, !d.added))
                if (d.added) {
                  Some(Retract(e, a, d.v, NoValue))
                } else {
                  Some(Add(e, a, d.v, NoValue))
                }
              }
            prevValue = v
            stmt
          }
          val txR                   = conn.transact(Seq(stmts))
          val (newT, newTx, newTxI) = (txR.t, txR.tx, date2strLocal(txR.inst))
          newTxs(txIndex) = (
            newT, newTx, newTxI,
            ListBuffer.empty[DatomTuple],
            datoms.sortBy(t => (t._1, t._2, t._4, t._3))
          )
          // Bit-encode newT/undoneT
          undoneTs += newT << 32 | undoneT
          txIndex += 1
        }

        // Add newT/undoneT pairs to meta db
        val moleculeAdminConn = Conn(base + "/MoleculeAdmin")
        val dbSettingsId      = user_User.username_("admin")
          .DbSettings.e.Db.name_(db).get(moleculeAdminConn)
        user_DbSettings(dbSettingsId).undoneTs.assert(undoneTs).update(moleculeAdminConn)

        Right(newTxs)
      } catch {
        case t: Throwable => Left(t.getMessage)
      }
    }(conn)
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
          date2strLocal(txInstant),
          op,
          attr,
          formatValue(conn, attr, v, enumAttrs)
        )
    }
  }

  override def getTFromTx(tx: Long): Long = Peer.toT(tx)

  override def getTxFromT(t: Long): Long = Peer.toTx(t).asInstanceOf[Long]

  override def getTTxFromTxInstant(db: String, txInstantStr: String): (Long, Long) = {
    val rawConn = Conn(base + "/" + db).datomicConn
    val result  = datomic.Peer.q(
      """[:find  ?t ?tx
        | :in    $ ?txInstant
        | :where [?tx :db/txInstant ?txInstant]
        |        [(datomic.Peer/toT ^Long ?tx) ?t]]""".stripMargin,
      rawConn.db(),
      strLocal2date(txInstantStr).asInstanceOf[Object]
    )
    val row     = result.iterator().next()
    (row.get(0).asInstanceOf[Long], row.get(1).asInstanceOf[Long])
  }

//  def withTransactor[T](
//    body: => Either[String, T]
//  )(implicit conn: Conn): Either[String, T] = try {
//    // Check if transactor responds by sending a Future back
//    conn.datomicConn.sync()
//    // Execute body of work
//    body
//  } catch {
//    case _: Throwable => Left(
//      "Datomic Transactor unavailable. Please restart it and try the operation again.")
//  }


  override def upsertQuery(db: String, query: QueryDTO): Either[String, String] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
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
    implicit val conn = Conn(base + "/MoleculeAdmin")
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
    implicit val conn = Conn(base + "/MoleculeAdmin")
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


  override def saveSettings(pairs: Seq[(String, String)]): Either[String, String] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
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

  def getCaster(tpe: String, enumPrefix: String): String => Any = {
    tpe match {
      case "String"               => (v: String) => enumPrefix + v
      case "Int" | "Long" | "ref" => (v: String) => v.toLong
      case "Float" | "Double"     => (v: String) => v.toDouble
      case "Boolean"              => (v: String) => v.toBoolean
      case "Date"                 => (v: String) => str2date(v)
      case "UUID"                 => (v: String) => UUID.fromString(v)
      case "URI"                  => (v: String) => new URI(v)
      case "BigInt"               => (v: String) => BigInt(v)
      case "BigDecimal"           => (v: String) => BigDecimal(v)
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
        Right((txR.t, txR.tx, date2strLocal(txR.inst)))
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
    rowValues: Seq[Seq[String]]
  ): Either[String, Long] = {
    // Model without initial entity id
    val elements = new Molecule2Model(molecule, nsMap).getModel.right.get.collect {
      case a: Atom => a
      case b: Bond => b
    }
    //    println(rowValues)
    //    elements foreach println
    implicit val conn = Conn(base + "/" + db)
    var i              = 0
    val data: Seq[Any] = elements.collect {
      case Atom(_, _, tpe, card, _, _, _, _) =>
        val cast = getCaster(tpe, "")
        val vs   = rowValues(i)
        i += 1
        if (vs.isEmpty) {
          None
        } else {
          // value/Seq/Map depending on cardinality
          card match {
            case 1 => cast(vs.head)
            case 2 => vs.map(cast).toList
            case 3 => vs.map { str =>
              val List(k, v) = str.split("__~~__", 2).toList
              k -> v
            }.toMap
          }
        }
    }

    val stmtss = Model2Transaction(conn, Model(elements)).insertStmts(Seq(data))
    println(data)
    stmtss.head foreach println
    withTransactor {
      try {
        Right(conn.transact(stmtss).eid)
        //        Left("going again...")
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
