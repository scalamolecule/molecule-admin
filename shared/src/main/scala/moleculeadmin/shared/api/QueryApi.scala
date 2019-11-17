package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.{Col, SavedQuery, QueryResult}
import scala.collection.mutable.ListBuffer

trait QueryApi extends BaseApi {

  def query(db: String,
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

  def getTxData(db: String,
                tx: Long,
                enumAttrs: Seq[String]
               ): Array[(Long, String, String, Boolean)] = ???

  def getEntityHistory(db: String,
                       eid: Long,
                       enumAttrs: Seq[String])
  : List[(Long, Long, String, Boolean, String, String)] = ???

  def addQuery(db: String, savedQuery: SavedQuery): Either[String, String] = ???

  def retractQuery(db: String, favMolecule: String): Either[String, String] = ???

  def saveViewSettings(openViews: Seq[String]): Either[String, String] = ???

  def saveMaxRowsSetting(maxRows: Int): Either[String, String] = ???

  def saveLimitSetting(limit: Int): Either[String, String] = ???

  def updateStr(db: String,
                attrFull: String,
                attrType: String,
                enumPrefix: String = "",
                data: Seq[(Long, Seq[String], Seq[String])]
               ): Either[String, (Long, Long, String)] = ???

  def updateNum(db: String,
                attrFull: String,
                attrType: String,
                data: Seq[(Long, Seq[Double], Seq[Double])]
               ): Either[String, (Long, Long, String)] = ???

}
