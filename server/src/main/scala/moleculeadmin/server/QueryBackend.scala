package moleculeadmin.server

import java.util.{Date, List => jList}
import datomic.{Datom, Peer}
import db.admin.dsl.moleculeAdmin._
import db.core.dsl.coreTest.Ns
import molecule.api.Entity
import molecule.api.out10._
import molecule.ast.model.{Atom, Bond, Model, NoValue}
import molecule.ast.transactionModel.{Add, Retract, RetractEntity, Statement}
import molecule.facade.{Conn, TxReport}
import molecule.transform.Model2Transaction
import moleculeadmin.server.query.{Rows2QueryResult, ToggleBackend}
import moleculeadmin.shared.ast.query.{Col, QueryDTO, QueryResult}
import moleculeadmin.shared.ops.transform.Molecule2Model
import org.slf4j.{Logger, LoggerFactory}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.util.Try


class QueryBackend extends ToggleBackend {

  val log: Logger = LoggerFactory.getLogger(getClass)

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
    log.info("\n---- Querying Datomic... --------------------\n" + datalogQuery)
    val t           = Timer("Query")
    val conn        = Conn(base + "/" + db)
    val allInputs   = if (rules.isEmpty)
      conn.db +: inputs(l ++ ll ++ lll)
    else
      conn.db +: rules.get +: inputs(l ++ ll ++ lll)
    val allRows     = Peer.q(datalogQuery, allInputs: _*)
    val queryTime   = t.delta
    val rowCountAll = allRows.size
    val rowCount    = if (maxRows == -1 || rowCountAll < maxRows)
      rowCountAll else maxRows

    log.info("Query time : " + thousands(queryTime) + " ms")
    log.info("rowCountAll: " + rowCountAll)
    log.info("maxRows    : " + (if (maxRows == -1) "all" else maxRows))
    log.info("rowCount   : " + rowCount)
    allRows.asScala.take(10).foreach(row => log.info(row.toString))

    if (rowCount == 0)
      Left(Nil)
    else {
      val queryResult = Rows2QueryResult(
        allRows, rowCountAll, rowCount, cols, queryTime).get

      log.info("Rows2QueryResult took " + t.ms)
      log.info("Sending data to client... Total server time: " + t.msTotal)
      Right(queryResult)
    }
  } catch {
    case t: Throwable =>
      Left(t.getMessage +: t.getStackTrace.toSeq.map(_.toString))
  }


  override def touchEntity(db: String, eid: Long): List[(String, String)] = {
    val conn = Conn(base + "/" + db)
    Entity(conn.db.entity(eid), conn, eid.asInstanceOf[Object])
      .touchListMax(1)
      .map {
        case (a, date: Date) => (a, date2strLocal(date))
        case (a, vs: Seq[_]) =>
          if (vs.nonEmpty && vs.head.isInstanceOf[Date])
            (a, vs.map(v =>
              date2strLocal(v.asInstanceOf[Date])
            ).mkString("__~~__"))
          else
            (a, vs.mkString("__~~__"))

        case (a, v) => (a, v.toString)
      }
  }

  override def getTxData(
    db: String,
    tx: Long,
    enumAttrs: Seq[String]
  ): (String, List[DatomTuple], List[DatomTuple]) = {
    val conn               = Conn(base + "/" + db)
    val result             = datomic.Peer.q(
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
    var txInst             = ""
    var txMetaData         = List.empty[DatomTuple]
    var txData             = List.empty[DatomTuple]
    val it                 = result.iterator()
    var row: jList[AnyRef] = null
    var e                  = 0L
    var attr               = ""
    var tpl: DatomTuple    = null
    while (it.hasNext) {
      row = it.next()
      e = row.get(0).asInstanceOf[Long]
      attr = row.get(1).asInstanceOf[String]
      tpl = (
        e,
        attr,
        formatValue(conn, attr, row.get(2), enumAttrs),
        row.get(3).asInstanceOf[Boolean]
      )
      if (e == tx) {
        if (attr == ":db/txInstant") {
          txInst = tpl._3
        } else {
          txMetaData = tpl :: txMetaData
        }
      } else {
        txData = tpl :: txData
      }
    }
    (
      txInst,
      txMetaData.sortBy(t => (t._1, t._2, t._4, t._3)),
      txData.sortBy(t => (t._1, t._2, t._4, t._3))
    )
  }

  override def getLastTxs(
    db: String,
    prevFirstT0: Long,
    enumAttrs: Seq[String]
  ): Either[String, Array[TxResult]] = {
    try {
      log.info("----------------------------")
      log.info("Fetching log transactions...")
      val timer      = Timer()
      val conn       = Conn(base + "/" + db)
      val datomicDb  = conn.db
      val prevFirstT = if (prevFirstT0 == 0L) datomicDb.basisT() else prevFirstT0
      val firstT     = if (prevFirstT > 1100) prevFirstT - 100 else 1001
      log.info("Fetching from t " + firstT)
      val txMaps = conn.datomicConn.log.txRange(firstT, null).asScala
      log.info("Fetched last " + txMaps.size + " txs from log in " + timer.ms)
      val txData        = new Array[TxResult](txMaps.size)
      var txIndex       = 0
      var t             = 0L
      var tx            = 0L
      var txInstant     = ""
      var txRaw: AnyRef = null
      var eRaw : AnyRef = null
      var e             = 0L
      var a             = ""
      var v             = ""
      var op            = true
      var first         = true
      var isData        = true
      var valid         = true

      txMaps.foreach { txMap =>
        t = txMap.get(datomic.Log.T).asInstanceOf[Long]
        val txMetaDatoms = new ListBuffer[DatomTuple]
        val dataDatoms   = new ListBuffer[DatomTuple]
        first = true
        isData = true
        valid = true

        txMap.get(datomic.Log.DATA).asInstanceOf[jList[Datom]].forEach { d =>
          eRaw = d.e
          e = eRaw.asInstanceOf[Long]
          a = datomicDb.ident(d.a).toString
          v = formatValue(conn, a, d.v, enumAttrs)
          op = d.added
          if (first) {
            txRaw = eRaw
            tx = e
            txInstant = v
            txMetaDatoms.+=((e, a, v, op))
            first = false
          } else if (isData && eRaw == txRaw) {
            txMetaDatoms.+=((e, a, v, op))
            isData = false
          } else if (isData) {
            if (a.startsWith(":db.") || a.startsWith(":db/"))
              valid = false
            // For some reason Datomic txMaps sometimes contain duplicate datoms
            // We don't want to show them twice
            if (!dataDatoms.contains((e, a, v, op)))
              dataDatoms.+=((e, a, v, op))
          } else {
            txMetaDatoms.+=((e, a, v, op))
          }
        }

        if (valid && dataDatoms.nonEmpty) {
          txData(txIndex) = (t, tx, txInstant, txMetaDatoms, dataDatoms)
          txIndex += 1
        }
      }
      // Return data transactions (without empty txs)
      val txData1 = new Array[TxResult](txIndex)
      System.arraycopy(txData, 0, txData1, 0, txIndex)
      log.info("Organized tx data in " + timer.ms)
      log.info("Sending tx data to client...")
      Right(txData1)
    } catch {
      case t: Throwable => Left(t.getMessage)
    }
  }

  override def undoTxs(
    db: String,
    ts: Seq[Long],
    enumAttrs: Seq[String]
  ): Either[String, Array[TxResult]] = {
    log.info("==============================")
    log.info(s"Undoing txs from t ${ts.head}...")
    val timer     = Timer()
    val dbConn    = Conn(base + "/" + db)
    val datoms    = new ListBuffer[DatomTuple]
    val undoneTs  = new ListBuffer[Long]
    var txIndex   = 0
    var e         = 0L
    var a         = ""
    var v         = ""
    var prevValue = ""
    withTransactor {
      val txMaps =
        if (ts.length == 1) {
          dbConn.datomicConn.log.txRange(ts.head, ts.head + 1).asScala.toList
        } else {
          dbConn.datomicConn.log.txRange(ts.head, null).asScala.toList
            .filter(txMap =>
              ts.contains(txMap.get(datomic.Log.T).asInstanceOf[Long]))
            .reverse // Undo transactions backwards
        }
      log.info("Fetched targeted " + txMaps.length + " txs in " + timer.ms)
      val newTxs = new Array[TxResult](txMaps.size)
      txMaps.foreach { txMap =>
        val undoneT   = txMap.get(datomic.Log.T).asInstanceOf[Long]
        val tx        = Peer.toTx(undoneT).asInstanceOf[Long]
        val rawDatoms = txMap.get(datomic.Log.DATA)
          .asInstanceOf[jList[Datom]].asScala.distinct

        datoms.clear()
        val stmts                 = rawDatoms.flatMap { d =>
          e = d.e.asInstanceOf[Long]
          a = dbConn.db.ident(d.a).toString
          v = formatValue(dbConn, a, d.v, enumAttrs)
          val stmt: Option[Statement] =
            if (
              a == ":db/txInstant" || e == tx ||
                Try(prevValue.toLong).isSuccess && e == prevValue.toLong
            ) {
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
        val txR                   = dbConn.transact(Seq(stmts.toSeq))
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
      log.info("Saved reversing txs in " + timer.ms)

      // Add newT/undoneT pairs to meta db
      val conn         = Conn(base + "/MoleculeAdmin")
      val dbSettingsId = user_User.username_("admin")
        .DbSettings.e.Db.name_(db).get(conn)
      user_DbSettings(dbSettingsId).undoneTs.assert(undoneTs).update(conn)

      log.info("Saved internal meta data in " + timer.ms)
      log.info("Sending reversing txs to client...")
      Right(newTxs)
    }(dbConn)
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

  override def getTTxFromTxInstant(
    db: String,
    txInstantStr: String
  ): (Long, Long) = {
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

  override def upsertQuery(db: String, query: QueryDTO): Either[String, String] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    withTransactor {
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
            user_Query.molecule.part.ns.isFavorite.showGrouped.groupedCols
              .ColSettings.*(
              user_ColSetting.colIndex.sortDir.sortPos.filterExpr
            ).insert(List(
              (
                query.molecule,
                query.part,
                query.ns,
                query.isFavorite,
                query.showGrouped,
                query.groupedColIndexes,
                query.colSettings.map(cs =>
                  (cs.colIndex, cs.sortDir, cs.sortPos, cs.filterExpr)
                )
              )
            )).eid

          user_DbSettings(dbSettingsId).queries.assert(newQueryId).update
          Right("Successfully inserted query")

        case Seq(queryId) =>
          // Re-insert col settings
          user_Query(queryId).colSettings().update
          val colSettingIds =
            user_ColSetting.colIndex.sortDir.sortPos.filterExpr.insert(
              colSettings.map(cs =>
                (cs.colIndex, cs.sortDir, cs.sortPos, cs.filterExpr)
              )
            ).eidSet

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
    }
  }


  override def updateQuery(db: String, query: QueryDTO): Either[String, String] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    withTransactor {
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
          user_ColSetting.colIndex.sortDir.sortPos.filterExpr.insert(
            colSettings.map(cs =>
              (cs.colIndex, cs.sortDir, cs.sortPos, cs.filterExpr)
            )
          ).eidSet
        user_Query(queryId)
          .isFavorite(isFavorite)
          .showGrouped(showGrouped)
          .groupedCols(groupedCols)
          .colSettings(colSettingIds)
          .update

        Right("ok")
      }
    }
  }

  override def retractQuery(db: String, query: QueryDTO): Either[String, String] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    val molecule1 = query.molecule
    withTransactor {
      user_User.username_("admin")
        .DbSettings.Db.name_(db)
        ._user_DbSettings.Queries.e.molecule_(molecule1)
        .get match {
        case Nil           => Left(
          s"Unexpectedly couldn't find saved molecule `$molecule1` in meta database."
        )
        case favIds        =>
          retract(favIds)
          Right("ok")
      }
    }
  }


  override def saveSettings(pairs: Seq[(String, String)]): Either[String, String] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    withTransactor {
      // Use admin for now
      val userId = user_User.e.username_("admin").get match {
        case List(eid) => eid
        case Nil       => user_User.username("admin").save.eid
      }
      // Save key/value settings
      user_User(userId).settings.assert(pairs).update
      Right("ok")
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
      if (stmtss.length < 1000) {
        val txR: TxReport = conn.transact(stmtss)
        Right((txR.t, txR.tx, date2strLocal(txR.inst)))
      } else {
        var first             = 1
        var last              = 1
        var lastTxR: TxReport = null
        log.info("Transacting " + stmtss.length + " statements:")
        stmtss.grouped(1000).foreach { stmtGroup =>
          lastTxR = conn.transact(stmtGroup)
          last = first + stmtGroup.length - 1
          log.info(s"$first - $last")
          first = last + 1
        }
        Right((lastTxR.t, lastTxR.tx, date2strLocal(lastTxR.inst)))
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

  import moleculeadmin.shared.ast.metaSchema

  override def insert(
    db: String,
    molecule: String,
    nsMap: Map[String, metaSchema.MetaNs],
    rowValues: Seq[Seq[String]]
  ): Either[String, Long] = {
    // Model without initial entity id
    val elements = new Molecule2Model(molecule, nsMap).getModel.getOrElse(Nil).collect {
      case a: Atom => a
      case b: Bond => b
    }
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
    log.info(data.toString)
    stmtss.head.foreach(smts => log.info(smts.toString))
    withTransactor {
      Right(conn.transact(stmtss).eid)
    }
  }

  override def retractEntities(
    db: String,
    eids: Array[Long]
  ): Either[String, Long] = {
    implicit val conn = Conn(base + "/" + db)
    val stmtss = Seq(eids.toSeq.map(RetractEntity))
    withTransactor {
      val txR: TxReport = conn.transact(stmtss)
      Right(txR.tx)
    }
  }

  // todo: check empty refs and create new in one tx fn to guarantee atomicity
  override def createJoins(
    db: String,
    eids: Seq[Long],
    nsFull: String,
    refAttr: String,
    refCard: Int,
    refNs: String,
    valueAttr: String,
    attrType: String,
    isEnum: Boolean,
    value: String
  ): Either[String, Int] = {
    implicit val conn = Conn(base + "/" + db)
    val refAttrFull  = s":$nsFull/$refAttr"
    val eligibleEids = if (refCard == 1) {
      // Don't overwrite existing card-one refs
      conn.q(
        s"""[:find  ?e
           | :in    $$ [?e ...]
           | :where (not [?e $refAttrFull])]""".stripMargin,
        eids
      ).map(_.head.toString.toLong)
    } else {
      // Add ref to card-many refs
      eids
    }
    withTransactor {
      if (eligibleEids.isEmpty) {
        Left("All entities already have card-one joins to attribute ``")
      } else {
        val part = if (nsFull.contains('_'))
          ":" + nsFull.split('_')(0)
        else
          ":db.part/user"

        val castedValue = if (isEnum)
          s":$refNs.$valueAttr/$value"
        else
          getCaster(attrType, "")(value)

        val stmtss = eligibleEids.map { eid =>
          val refId = Peer.tempid(part)
          Seq(
            Add(eid, refAttrFull, refId, NoValue),
            Add(refId, s":$refNs/$valueAttr", castedValue, NoValue)
          )
        }
        conn.transact(stmtss)
        Right(stmtss.length)
      }
    }
  }


  override def getBackRefsData(
    db: String,
    eid: Long
  ): Either[String, ListBuffer[(String, Int)]] = {
    try {
      val raw  = Peer.q(
        s"""[:find  ?attrName (count ?backRef)
           | :in $$ ?eid
           | :where [?backRef ?attrId ?eid]
           |        [?attrId :db/ident ?attrIdent]
           |        [(str ?attrIdent) ?attrName]]""".stripMargin,
        Conn(base + "/" + db).db,
        eid.asInstanceOf[Object]
      )
      val data = new ListBuffer[(String, Int)]
      val it   = raw.iterator()
      while (it.hasNext) {
        val row = it.next
        data.+=((row.get(0).toString, row.get(1).toString.toInt))
      }
      Right(data)
    } catch {
      case t: Throwable => Left(t.getMessage)
    }
  }


  override def upsertEditExpr(
    db: String,
    fullAttr: String,
    editExpr: String
  ): Either[String, String] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    withTransactor {
      // Use admin for now
      val userId: Long = user_User.e.username_("admin").get match {
        case List(eid) => eid
        case Nil       => user_User.username("admin").save.eid
      }
      val dbId  : Long = meta_Db.e.name_(db).get.headOption match {
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

      val time = System.currentTimeMillis()
      user_DbSettings(dbSettingsId)
        .Edits.e.attr_(fullAttr).expr_(editExpr)
        .get match {
        case Nil =>
          val newId = user_EditExpr.attr(fullAttr).time(time).expr(editExpr).save.eid
          user_DbSettings(dbSettingsId).edits.assert(newId).update
          Right(s"Successfully inserted edit expression `$editExpr`")

        case Seq(exprId) =>
          // Update timestamp of existing expr
          user_EditExpr(exprId).time(time).update
          Right(s"Successfully updated edit expr `$editExpr`")

        case exprIds => Left(
          s"Unexpectedly found edit expression `$editExpr` ${exprIds.length} times.")
      }
    }
  }

  override def retractEditExpr(
    db: String,
    fullAttr: String,
    editExpr: String
  ): Either[String, String] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    withTransactor {
      user_User.username_("admin")
        .DbSettings.Db.name_(db)
        ._user_DbSettings.Edits.e.attr_(fullAttr).expr_(editExpr)
        .get match {
        case Nil              => Left(
          s"Unexpectedly couldn't find saved edit expression `$editExpr` " +
            s"for attr `$fullAttr` in meta database."
        )
        case List(editExprId) =>
          editExprId.retract
          Right(s"Successfully retracted edit expression `$editExpr`")
        case favIds           =>
          Left(s"Unexpectedly found ${favIds.size} instances of saved " +
            s"edit expression `$editExpr` for attr `$fullAttr` in meta database.")
      }
    }
  }
}
