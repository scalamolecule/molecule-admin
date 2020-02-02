package moleculeadmin.server.query

import molecule.facade.Conn
import moleculeadmin.server.Base
import moleculeadmin.server.utils.DateStrLocal
import moleculeadmin.shared.api.QueryApi


trait QueryBase extends QueryApi with Base with DateStrLocal {


  def withTransactor[T](
    body: => Either[String, T]
  )(implicit conn: Conn): Either[String, T] = try {
    // Check if transactor responds by sending a Future back
    conn.datomicConn.sync()
    // Execute body of work
    body
  } catch {
    case _: Throwable => Left(
      "Datomic Transactor unavailable. Please restart it and try the operation again.")
  }
}
