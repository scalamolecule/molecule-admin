package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.{Col, Favorite, QueryResult}
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

  def addFavorite(db: String, favorite: Favorite): Either[String, String] = ???

  def retractFavorite(db: String, favMolecule: String): Either[String, String] = ???

  def saveSnippetSettings(openSnippets: Seq[String]): Either[String, String] = ???

  def updateStr(db: String,
                attrFull: String,
                attrType: String,
                data: Seq[(Long, Seq[String], Seq[String])],
                enumPrefix: String = "",
               ): Either[String, (Long, Long, String)] = ???

  def updateNum(db: String,
                attrFull: String,
                attrType: String,
                data: Seq[(Long, Seq[Double], Seq[Double])],
               ): Either[String, (Long, Long, String)] = ???

}
