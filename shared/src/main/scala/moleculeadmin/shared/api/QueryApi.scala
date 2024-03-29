package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.{Col, QueryDTO, QueryResult}
import moleculeadmin.shared.ast.metaSchema
import scala.collection.mutable.ListBuffer

trait QueryApi extends BaseApi {

  type DatomTuple = (Long, String, String, Boolean) // e a v op
  type TxResult = (
    Long, Long, String, // t, tx, txInstant (as String)
      ListBuffer[DatomTuple], // tx meta datoms
      ListBuffer[DatomTuple] // data datoms
    )

  def query(
    db: String,
    datalogQuery: String,
    rules: Option[String],
    l: Seq[(Int, (String, String))],
    ll: Seq[(Int, Seq[(String, String)])],
    lll: Seq[(Int, Seq[Seq[(String, String)]])],
    maxRows: Int,
    cols: Seq[Col]
  ): Either[Seq[String], QueryResult] = ???

  def touchEntity(db: String, eid: Long): List[(String, String)] = ???

  def getTFromTx(t: Long): Long = ???
  def getTxFromT(t: Long): Long = ???
  def getTTxFromTxInstant(db: String, txInstantStr: String): (Long, Long) = ???

  def getTxData(
    db: String,
    tx: Long,
    enumAttrs: Seq[String]
  ): (String, List[DatomTuple], List[DatomTuple]) = ???

  def getLastTxs(
    db: String,
    prevFirstT: Long,
    enumAttrs: Seq[String]
  ): Either[String, Array[TxResult]] = ???

  def undoTxs(
    db: String,
    ts: Seq[Long],
    enumAttrs: Seq[String]
  ): Either[String, Array[TxResult]] = ???

  def getEntityHistory(
    db: String,
    eid: Long,
    enumAttrs: Seq[String]
  ): List[(Long, Long, String, Boolean, String, String)] = ???


  def upsertQuery(db: String, query: QueryDTO): Either[String, String] = ???
  def updateQuery(db: String, query: QueryDTO): Either[String, String] = ???
  def retractQuery(db: String, query: QueryDTO): Either[String, String] = ???

  def saveToggle(
    db: String,
    dbSettingsIdOpt: Option[Long],
    markerType: String,
    eids: collection.Set[Long],
    newState: Boolean
  ): Either[String, Long] = ???

  def saveSettings(pairs: Seq[(String, String)]): Either[String, String] = ???

  def updateStr(
    db: String,
    attrFull: String,
    attrType: String,
    enumPrefix: String = "",
    data: Seq[(Long, Seq[String], Seq[String])]
  ): Either[String, (Long, Long, String)] = ???

  def updateNum(
    db: String,
    attrFull: String,
    attrType: String,
    data: Seq[(Long, Seq[Double], Seq[Double])]
  ): Either[String, (Long, Long, String)] = ???

  def insert(
    db: String,
    molecule: String,
    nsMap: Map[String, metaSchema.MetaNs],
    rowValues: Seq[Seq[String]]
  ): Either[String, Long] = ???

  def retractEntities(db: String, eids: Array[Long]): Either[String, Long] = ???

  def createJoins(
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
  ): Either[String, Int] = ???

  def getBackRefsData(
    db: String,
    eid: Long
  ): Either[String, ListBuffer[(String, Int)]] = ???

  def upsertEditExpr(
    db: String,
    fullAttr: String,
    editExpr: String
  ): Either[String, String] = ???

  def retractEditExpr(
    db: String,
    fullAttr: String,
    editExpr: String
  ): Either[String, String] = ???
}
