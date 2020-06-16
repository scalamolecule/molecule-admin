package moleculeadmin.server.query

import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.facade.{Conn, TxReport}


trait ToggleBackend extends QueryHelpers {

  private def save[A, B](
    action: String,
    eids: collection.Set[Long],
    update: collection.Set[Long] => TxReport
  )(implicit conn: Conn): Unit = {
    val t = Timer(action)
    if (eids.size > 10000) {
      println(s"$action ${eids.size} entities ...")
      var count = 0
      eids.sliding(10000, 10000).foreach { eidsChunk =>
        count += eidsChunk.size
        update(eidsChunk)
        t.log(count)
      }
      t.total
    } else {
      update(eids)
    }
  }

  override def saveToggle(
    db: String,
    dbSettingsIdOpt: Option[Long],
    markerType: String,
    eids: collection.Set[Long],
    currentlyOn: Boolean
  ): Either[String, Long] = {
    implicit val conn = Conn(base + "/MoleculeAdmin")
    withTransactor {
      val dbSettingsId = dbSettingsIdOpt.getOrElse {
        val userId = user_User.e.username_("admin").get match {
          case List(eid) => eid
          case Nil       => user_User.username("admin").save.eid
        }
        user_User(userId).DbSettings.e.Db.name_(db).get match {
          case List(dbSettingsId) => dbSettingsId
          case Nil                =>
            val dbId = meta_Db.e.name_(db).get.head
            user_DbSettings.db(dbId).save.eid
        }
      }
      if (currentlyOn) {
        markerType match {
          case "star" => save("Unstar", eids, (eids: collection.Set[Long]) =>
            user_DbSettings(dbSettingsId).stars.retract(eids).update)

          case "flag" => save("Unflag", eids, (eids: collection.Set[Long]) =>
            user_DbSettings(dbSettingsId).flags.retract(eids).update)

          case "check" => save("Uncheck", eids, (eids: collection.Set[Long]) =>
            user_DbSettings(dbSettingsId).checks.retract(eids).update)
        }
      } else {
        markerType match {
          case "star" => save("Star", eids, (eids: collection.Set[Long]) =>
            user_DbSettings(dbSettingsId).stars.assert(eids).update)

          case "flag" => save("Flag", eids, (eids: collection.Set[Long]) =>
            user_DbSettings(dbSettingsId).flags.assert(eids).update)

          case "check" => save("Check", eids, (eids: collection.Set[Long]) =>
            user_DbSettings(dbSettingsId).checks.assert(eids).update)
        }
      }
      Right(dbSettingsId)
    }
  }
}
