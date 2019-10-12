package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.Favorite
import moleculeadmin.shared.ast.schema.MetaSchema
import moleculeadmin.shared.ast.query.Favorite
import moleculeadmin.shared.ast.schema._

trait BaseApi {

  def base() = "datomic:free://localhost:4334"

  def dbNames(): Seq[String] = ???

  def loadMetaData(db: String): (Seq[String], MetaSchema, (Set[String], Seq[Favorite])) = ???

  def getMetaSchema(db: String): MetaSchema = ???

  def getFlatMetaSchema(db: String): FlatSchema = ???
}
