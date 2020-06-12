package moleculeadmin.servertest

import molecule.facade.Conn
import moleculeadmin.server.Schema

class Partition1Setup extends Schema with ResetDbsCmds {
  val moleculeAdminConn = resetDbs(Seq("CoreTest", "Partition1"))
  lazy val coretestConn   = Conn(base + "/CoreTest")
  lazy val partitionConn  = Conn(base + "/Partition")
  lazy val partition1Conn = Conn(base + "/Partition1")
}
