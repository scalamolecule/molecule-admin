package moleculeadmin.servertest

import molecule.facade.Conn
import moleculeadmin.server.Schema

class PartitionSetup extends Schema with ResetDbsCmds {
  val moleculeAdminConn = resetDbs(Seq("CoreTest", "Partition"))
  lazy val coretestConn   = Conn(base + "/CoreTest")
  lazy val partitionConn  = Conn(base + "/Partition")
  lazy val partition1Conn = Conn(base + "/Partition1")
}
