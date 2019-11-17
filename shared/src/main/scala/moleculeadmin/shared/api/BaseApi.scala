package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.SavedQuery
import moleculeadmin.shared.ast.schema.{MetaSchema, _}

trait BaseApi {

  def base() = "datomic:free://localhost:4334"

  def dbNames(): Seq[String] = ???

  def loadMetaData(db: String): (Seq[String], MetaSchema, (Set[String], Seq[SavedQuery])) = ???

  def getMetaSchema(db: String): MetaSchema = ???

  def getFlatMetaSchema(db: String): FlatSchema = ???
}
