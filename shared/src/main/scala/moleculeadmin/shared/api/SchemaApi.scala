package moleculeadmin.shared.api

import moleculeadmin.shared.ast.metaSchema._

trait SchemaApi extends BaseApi {

  def getLiveSchema(db: String): FlatSchema = ???

  def updateSchemaCounts(db: String): Unit = ???

  def updateDescrAttr(
    db: String, part: String, ns: String, attr: String, descrAttr: Option[String]
  ): FlatSchema = ???

  def getFlatSchemas(db: String): (FlatSchema, FlatSchema, FlatSchema) = ???

  def getSchemas(db: String): (FlatSchema, FlatSchema, FlatSchema, MetaSchema) = ???

  def getSchemas2(db: String): (Seq[String], FlatSchema, MetaSchema) = ???

  def createPartition(
    schema: MetaSchema, db: String, part: String,
    descr: Option[String] = None,
    pos0: Int = 0
  ): Either[String, MetaSchema] = ???

  def updatePartition(
    schema: MetaSchema, db: String, curPart: String, newPart: String,
    descr: Option[String] = None,
    pos0: Int = 0
  ): Either[String, MetaSchema] = ???

  def deletePartition(
    schema: MetaSchema, db: String, part: String
  ): Either[String, MetaSchema] = ???


  def createNamespace(
    schema: MetaSchema, db: String, part: String, ns: String,
    descr: Option[String] = None,
    pos0: Int = 0
  ): Either[String, MetaSchema] = ???

  def updateNamespace(
    schema: MetaSchema, db: String, part: String, curNs: String, newNs: String,
    descr: Option[String] = None,
    pos0: Int = 0
  ): Either[String, MetaSchema] = ???

  def deleteNamespace(
    schema: MetaSchema, db: String, part: String, ns: String
  ): Either[String, MetaSchema] = ???


  def createAttribute(
    schema: MetaSchema, db: String, part: String, ns: String, attr: String,
    card: Int, tpe: String,
    enums: Seq[String] = Nil,
    optRefNs: Option[String] = None,
    options: Seq[String] = Nil,
    doc: Option[String] = None,
    pos0: Int = 0): Either[String, MetaSchema] = ???

  def updateAttribute(
    schema: MetaSchema, db: String, part: String, ns: String,
    curAttr: String, newAttr: String,
    card: Int,
    tpe: String,
    enums: Seq[String] = Nil,
    optRefNs: Option[String] = None,
    options: Seq[String] = Nil,
    doc: Option[String] = None,
    pos0: Int = 0,
    attrGroup: Option[String] = None
  ): Either[String, MetaSchema] = ???

  def deleteAttribute(
    schema: MetaSchema, db: String, part: String, ns: String, attr: String
  ): Either[String, MetaSchema] = ???
}
