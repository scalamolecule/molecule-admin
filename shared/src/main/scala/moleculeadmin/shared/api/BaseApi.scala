package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.QueryDTO
import moleculeadmin.shared.ast.schema.{MetaSchema, _}

trait BaseApi {

  def dbProtocol() = "free"
  def dbHost() = "localhost:4334"
  def base() = s"datomic:$dbProtocol://$dbHost"

  def dbNames(): Seq[String] = ???

  def loadMetaData(db: String): (
    Seq[String],
      MetaSchema,
      (Map[String, String],
        Set[Long],
        Set[Long],
        Set[Long],
        Set[Long],
        Seq[QueryDTO]
        )) = ???

  def getMetaSchema(db: String): MetaSchema = ???

  def getFlatMetaSchema(db: String): FlatSchema = ???
}
