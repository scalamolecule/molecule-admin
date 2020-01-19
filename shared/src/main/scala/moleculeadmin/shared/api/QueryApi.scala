package moleculeadmin.shared.api

import molecule.ast.model.Model
import moleculeadmin.shared.ast.query.{Col, QueryDTO, QueryResult}
import moleculeadmin.shared.ast.schema
import scala.collection.mutable.ListBuffer

trait QueryApi extends BaseApi {

  type DatomTuple = (Long, String, String, Boolean) // e a v op
  type TxData = (
    Long, Long, String, // t, tx, txInstant (as String)
      ListBuffer[DatomTuple], // Tx meta datoms
      ListBuffer[DatomTuple] // datoms
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
  ): Array[DatomTuple] = ???

  def getLastTxs(
    db: String,
    noOfTxs: Int,
    enumAttrs: Seq[String]
  ): Array[TxData] = ???

  def undoTxs(
    db: String,
    txs: Array[TxData],
    firstT: Long,
    lastT: Long,
    enumAttrs: Seq[String]
  ): Either[String, Array[TxData]] = ???

  def getEntityHistory(
    db: String,
    eid: Long,
    enumAttrs: Seq[String]
  ): List[(Long, Long, String, Boolean, String, String)] = ???


  def upsertQuery(db: String, query: QueryDTO): Either[String, String] = ???
  def updateQuery(db: String, query: QueryDTO): Either[String, String] = ???
  def retractQuery(db: String, query: QueryDTO): Either[String, String] = ???

  def toggleMarker(
    db: String,
    dbSettingsIdOpt: Option[Long],
    tpe: String,
    eid: Long,
    isOn: Boolean
  ): Either[String, Long] = ???

  def setMarkers(
    db: String,
    dbSettingsIdOpt: Option[Long],
    tpe: String,
    eids: Set[Long],
    newState: Boolean
  ): Either[String, Long] = ???

  def unmarkAll(
    db: String,
    dbSettingsIdOpt: Option[Long],
    tpe: String,
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
    nsMap: Map[String, schema.Ns],
    rowValues: Seq[Seq[String]]
  ): Either[String, Long] = ???

  def retractEntity(db: String, eid: Long): Either[String, Long] = ???
}
