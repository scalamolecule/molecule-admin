package moleculeadmin.server

import java.io.File
import datomic.Peer
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.server.utils.DefFile
import moleculeadmin.servertest._
import moleculeadmin.shared.api.DbsApi
import moleculeadmin.shared.ast.db.Db
import org.slf4j.LoggerFactory
import scala.jdk.CollectionConverters._

class Dbs extends DbsApi with Base with ResetDbsCmds {

  private val log = LoggerFactory.getLogger(getClass)

  def dbs()(implicit conn: Conn): Seq[Db] = {
    meta_Db.name.defFilePath$.get.sortBy(_._1).zipWithIndex.map {
      case ((name, df), i) => Db(i + 1, name, df.getOrElse(""), false, "", "")
    }
  }

  override def dbList: Either[Seq[String], Seq[Db]] = try {
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

    // Live databases (without MoleculeAdmin)
    val liveDbs = allDbs.filterNot(_ == "MoleculeAdmin")
    if (liveDbs.isEmpty)
      throw new RuntimeException("No live databases found")

    // Sync managed dbs with live databases
    val managedDbs    = meta_Db.name.get.sorted
    // add new
    val notManagedYet = liveDbs.diff(managedDbs)
    if (notManagedYet.nonEmpty) {
      log.info("Dbs not managed yet: " + notManagedYet)
      meta_Db.name(notManagedYet).save
    }
    // retract obsolete
    val notManagedAnymore = managedDbs.diff(liveDbs)
    if (notManagedAnymore.nonEmpty) {
      log.info("Dbs not managed anymore: " + notManagedAnymore)
      retract(meta_Db.e.name_(notManagedAnymore).get)
    }

    Right(dbs())

  } catch {
    case t: Throwable if t.toString.contains("Connection is broken") =>
      Left(List("transactor down"))
    case t: Throwable                                                =>
      Left(t.getMessage +: t.getStackTrace.toList.map(_.toString))
  }


  override def checkPath(db: String, path: String): Either[String, String] = {
    val defFile = new File(path)
    if (!defFile.isFile) {
      log.warn(s"Can't find definition file for db `$db` in path:\n$path")
      Left(s"Can't find definition file in path.")
    } else {
      log.info(s"Schema definition file path for db `$db` is ok:\n$path")
      Right("Schema definition file path ok.")
    }
  }

  override def skipManaging(db: String): Either[String, String] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    withTransactor {
      val dbId = meta_Db.e.name_(db).get.head
      meta_Db(dbId).defFilePath().partitions().update
      Right(s"Successfully skipped managing db `$db`.")
    }
  }

  override def saveDefFilePath(db: String, path: String): Either[String, String] = {
    val defFile = new File(path)
    if (!defFile.isFile) {
      log.warn(s"Can't find definition file for db `$db` in path:\n$path")
      Left(s"Can't find definition file in path.")
    } else {
      // We should have a schema definition file - save meta data
      DefFile(db, Some(path), Some(defFile)).saveToMetaDb match {
        case Left(err) =>
          log.error(s"Couldn't save schema definition path for db `$db`:\n$path")
          Left(err)
        case Right(_)  =>
          log.info(s"Successfully saved schema definition file path for db `$db`:\n$path")
          Right("Successfully saved path.")
      }
    }
  }

  override def createDb(newDbName: String): Either[String, Seq[Db]] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    withTransactor {
      val rawDbs = Peer.getDatabaseNames(s"$base/*")
      if (rawDbs.contains(newDbName))
        return Left(s"Database '$newDbName' already exists.")

      val newDbUri = base + "/" + newDbName
      if (!Peer.createDatabase(newDbUri))
        return Left(s"Database '$newDbName' was not created and no exception thrown, hmm.")

      val rawDbs2 = Peer.getDatabaseNames(s"$base/*")
      if (!rawDbs2.contains(newDbName))
        return Left(s"Newly created database '$newDbName' wasn't found among new list of databases.")

      meta_Db.name(newDbName).save
      Right(dbs())
    }
  }

  override def deleteDb(dbName: String): Either[String, Seq[Db]] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    withTransactor {
      // make sure we have the meta db before deleting the live database
      val dbId = meta_Db.e.name_(dbName).get.head

      val rawDbs = Peer.getDatabaseNames(s"$base/*")
      if (!rawDbs.contains(dbName))
        return Left(s"Couldn't find database '$dbName'.")

      val dbUri = base + "/" + dbName
      if (!Peer.deleteDatabase(dbUri))
        return Left(s"Database '$dbName' was not deleted and no exception thrown, hmm.")

      val rawDbs2 = Peer.getDatabaseNames(s"$base/*")
      if (rawDbs2.contains(dbName))
        return Left(s"Supposedly deleted database '$dbName' is unexpectedly " +
          s"still found among list of databases.")

      // Remove meta data about db
      dbId.retract
      Right(dbs())
    }
  }
}