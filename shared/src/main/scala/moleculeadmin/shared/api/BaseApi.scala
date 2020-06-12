package moleculeadmin.shared.api

import moleculeadmin.shared.ast.query.QueryDTO
import moleculeadmin.shared.ast.schema.MetaSchema


trait BaseApi {

  type SettingsMetaData = (
    Map[String, String],
      Set[Long],
      Set[Long],
      Set[Long],
      Set[Long],
      Seq[QueryDTO],
      Map[String, List[String]]
    )

  type PageMetaData = (Seq[String], MetaSchema, SettingsMetaData)

  def dbNames: Seq[String] = ???

  def loadMetaData(db: String): Either[String, PageMetaData] = ???

  def getMetaSchema(db: String): MetaSchema = ???
}
