package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.{Col, QueryDTO, QueryResult}
import scala.collection.mutable.ListBuffer

trait QueryApi extends BaseApi {

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
  ): Array[(Long, String, String, Boolean)] = ???

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

  def saveOpenViews(openViews: Seq[String]): Either[String, String] = ???

  def saveSetting(key: String, value: String): Either[String, String] = ???


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

}
