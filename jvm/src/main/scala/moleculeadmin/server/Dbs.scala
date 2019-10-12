package moleculeadmin.server

import java.io.File
import datomic.Peer
import db.admin.dsl.meta._
import db.admin.schema.MetaSchema
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.shared.api.DbsApi
//import moleculeadmin.shared.Dbs._
import moleculeadmin.server.utils.DefFile
import scala.collection.JavaConverters._

/*
UC1: Choose action on db

1. User request to see list of databases available
2. System validates live/meta db is in sync
3. System shows list of databases (from MetaDb)
4. User selects action on a database:
    - Browse/edit schema
    - Browse/edit data
5. System presents chosen action interface of db


1a. Transactor not started:
    1. System suggests User how to start transactor

2a. MetaDb hasn't been created:
    1. System creates MetaDb

2b. Live db is empty
    1. todo
    2. --> 2

2c. Live db is not in MetaDb:
    1. System creates meta db

2d. Def file not found:
    1. Ask User for new def file path

2e. Missing def file path:
    1. Ask User for new def file path

2f. Meta db schema is out of sync with Live db:
    1. todo
*/

class Dbs extends DbsApi {


  def dbs(): Dbs = {
    implicit val conn = metaConn
    meta_Db.name.isMolecular$.defFilePath$.get.sortBy(_._1)
  }

  def dbs_()(implicit conn: Conn): Dbs = {
    //    implicit val conn = metaConn
    meta_Db.name.isMolecular$.defFilePath$.get.sortBy(_._1)
  }

  def metaConn = Conn(base + "/meta")


  def dbList(): Either[List[String], List[(String, Option[Boolean], Option[String])]] = try {

    //    recreateDbFrom(MetaSchema, "localhost:4334/meta", "free")


    // 2. Prepare sync - get connection
    implicit val conn = try {
      metaConn
    } catch { // 2a
      case e: RuntimeException if e.toString.contains(s"Could not find meta in catalog") =>
        // Create meta database if absent
        recreateDbFrom(MetaSchema, "localhost:4334/meta", "free")
    }

    // 2. Sync meta/live dbs

    // Live database names
    val liveDbs = Peer.getDatabaseNames(s"$base/*").asScala.toList.filterNot(_ == "meta")

    if (liveDbs.isEmpty)
      throw new RuntimeException("No live databases found") // 2b

    // Meta database names
    val metaDbs = meta_Db.name.get

    // Include all live dbs
    for (liveDb <- liveDbs) {
      if (!metaDbs.contains(liveDb)) // 2c
        meta_Db.name(liveDb).save // 2c.1
    }

    // 3. Show list of databases in client
    Right(dbs_())

  } catch {
    case t: Throwable if t.toString.contains("Connection is broken") =>
      Left(List("transactor not running"))
    case t: Throwable                                                =>
      Left(t.getMessage +: t.getStackTrace.toList.map(_.toString))
  }


  // shared api ...............................................................

  def ignore(db: String): Dbs = {
    implicit val conn = metaConn
    val dbId = meta_Db.e.name_(db).get.head
    meta_Db(dbId).isMolecular(false).defFilePath().partitions().update
    dbs_()
  }
  def reset(db: String): Dbs = {
    implicit val conn = metaConn
    val dbId = meta_Db.e.name_(db).get.head
    meta_Db(dbId).isMolecular().defFilePath().partitions().update
    dbs_()
  }

  //  def resyncSchemaDef(db: String): Option[String] = {
  //    implicit val conn = Conn(base + "/meta")
  //    val path = meta_Db.name_(db).defFilePath.get.head
  //
  //    // Client validation
  //    validateDefFilePath(path) match {
  //      case Left(error)  => Some(error)
  //      case Right(path1) =>
  //        // Server validation
  //        saveDefFilePath(db, path1) match {
  //          case Left(error) => Some(error)
  //          case Right(_)    => None
  //        }
  //    }
  //  }

  //  // List of validated dbs
  //  def validatedDbs(): Dbs =
  //    dbs() map {
  //      // Confirmed non-molecular db
  //      case (db, Some(false), _) => (db, Some(false), None)
  //
  //      // Confirmed molecular db
  //      case (db, Some(true), Some(path)) =>
  //        val defFile = new File(path)
  //        if (!defFile.isFile) {
  //          // 2d: Def file not found
  //          (db, None, Some(s"Can't find: $path"))
  //        } else {
  //          // todo: check def file
  //          // We have a likely def file
  //          (db, Some(true), Some(path))
  //        }
  //      case (db, Some(true), None)       =>
  //        // 2e: Missing def file path - set isMolecular to None
  //        (db, None, None)
  //
  //      // Un-categorized db (could be non-molecular)
  //      case (db, _, _) => (db, None, None)
  //    }


  def saveDefFilePath(db: String, path: String): Either[String, Dbs] = {
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