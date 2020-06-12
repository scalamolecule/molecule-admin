package moleculeadmin.server

import java.io.File
import datomic.Peer
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.server.utils.DefFile
import moleculeadmin.servertest._
import moleculeadmin.shared.api.DbsApi
import org.slf4j.LoggerFactory
import scala.jdk.CollectionConverters._


class Dbs extends DbsApi with ResetDbsCmds {

  private val log = LoggerFactory.getLogger(getClass)

  def dbs(): Dbs = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    meta_Db.name.isMolecular$.defFilePath$.get.sortBy(_._1)
  }

  def dbs_()(implicit conn: Conn): Dbs = {
    meta_Db.name.isMolecular$.defFilePath$.get.sortBy(_._1)
  }

  override def dbList(): Either[List[String], List[(String, Option[Boolean], Option[String])]] = try {
    // Live database names
    val rawDbs = Peer.getDatabaseNames(s"$base/*")
    if (rawDbs == null)
      throw new RuntimeException("No databases found - maybe the mBrainz sample db wasn't restored?")
    val allDbs = rawDbs.asScala.toList.sorted

    implicit val conn = if (allDbs.contains("MoleculeAdmin")) {
      Conn(base + "/MoleculeAdmin")
    } else {
      val conn = resetDbs()
      populateCoreTest(Conn(base + "/CoreTest"))
      populatePartition(Conn(base + "/Partition"))
      populateTree(Conn(base + "/Tree"))
      conn
    }
    val nonMetaDbs = allDbs.filterNot(_ == "MoleculeAdmin")
    if (nonMetaDbs.isEmpty)
      throw new RuntimeException("No live databases found")
    val managedDbs = meta_Db.name.get.sorted

    // Include all live dbs
    for (liveDb <- nonMetaDbs) {
      if (!managedDbs.contains(liveDb))
        meta_Db.name(liveDb).save
    }
    Right(dbs_())

  } catch {
    case t: Throwable if t.toString.contains("Connection is broken") =>
      Left(List("transactor not running"))
    case t: Throwable                                                =>
      Left(t.getMessage +: t.getStackTrace.toList.map(_.toString))
  }

  override def reset(db: String): Dbs = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    val dbId = meta_Db.e.name_(db).get.head
    meta_Db(dbId).isMolecular().defFilePath().partitions().update
    dbs_()
  }

  override def saveDefFilePath(db: String, path: String): Either[String, Dbs] = {
    val defFile = new File(path)
    if (!defFile.isFile) {
      Left(s"Can't find definition file in path '$path'")
    } else {
      // We should have a Schema Definition file
      DefFile(db, Some(path), Some(defFile)).saveToMetaDb match {
        case Left(err) => Left(err)
        case Right(_)  => Right(dbs())
      }
    }
  }
}