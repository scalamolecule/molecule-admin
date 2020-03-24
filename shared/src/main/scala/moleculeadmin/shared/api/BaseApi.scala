package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.QueryDTO
import moleculeadmin.shared.ast.schema.{MetaSchema, _}

trait BaseApi {

  def dbProtocol() = "free"
  def dbHost() = "localhost:4334"
  def base() = s"datomic:$dbProtocol://$dbHost"

  def dbNames(): Seq[String] = ???

  type SettingsMetaData = (
    Map[String, String],
      Set[Long],
      Set[Long],
      Set[Long],
      Set[Long],
      Seq[QueryDTO]
    )

  type PageMetaData = (Seq[String], MetaSchema, SettingsMetaData)

  def loadMetaData(db: String): PageMetaData = ???

  def getMetaSchema(db: String): MetaSchema = ???

  def getFlatMetaSchema(db: String): FlatSchema = ???
}
