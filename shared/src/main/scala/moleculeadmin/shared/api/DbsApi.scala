package moleculeadmin.shared.api

trait DbsApi extends BaseApi {
  type Dbs = List[(String, Option[Boolean], Option[String])]

  def dbList(): Either[List[String], Dbs]
  def ignore(db: String): Dbs
  def reset(db: String): Dbs
  def saveDefFilePath(db: String, path: String): Either[String, Dbs]
}
