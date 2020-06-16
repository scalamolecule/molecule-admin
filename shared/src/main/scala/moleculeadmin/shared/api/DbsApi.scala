package moleculeadmin.shared.api

import moleculeadmin.shared.ast.db.Db

trait DbsApi extends BaseApi {
  def dbList: Either[Seq[String], Seq[Db]] = ???
  def saveDefFilePath(db: String, path: String): Either[String, String] = ???
  def checkPath(db: String, path: String): Either[String, String] = ???
  def skipManaging(db: String): Either[String, String] = ???
  def createDb(dbName: String): Either[String, Seq[Db]] = ???
  def deleteDb(dbName: String): Either[String, Seq[Db]] = ???
}
