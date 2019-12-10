package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.QueryData
import moleculeadmin.shared.ast.schema.{MetaSchema, _}

trait BaseApi {

  def base() = "datomic:free://localhost:4334"

  def dbNames(): Seq[String] = ???

  def loadMetaData(db: String): (
    Seq[String],
      MetaSchema,
      (Map[String, String],
        Set[String],
        Set[Long],
        Set[Long],
        Set[Long],
        Seq[QueryData]
        )) = ???

  def getMetaSchema(db: String): MetaSchema = ???

  def getFlatMetaSchema(db: String): FlatSchema = ???
}
