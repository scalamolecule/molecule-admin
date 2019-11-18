package moleculeadmin.client.app.domain.query.data.edit
import java.net.URI
import java.util.UUID
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.shared.util.HelpersAdmin
import scala.scalajs.js.Date
import scala.util.{Failure, Success, Try}

trait Update extends HelpersAdmin with KeyEvents {

  protected def valid(attrType: String, s: String): Boolean = if (s.isEmpty)
    true
  else
    attrType match {
      case "Int"        => s.isInt
      case "Long"       => s.isLong
      case "Float"      => s.isFloat
      case "Double"     => s.isDouble
      case "Boolean"    => s.isBoolean
      case "Date"       => Try(expandDateStr(s)).isSuccess
      case "UUID"       => Try(UUID.fromString(s)).isSuccess
      case "URI"        => Try(new URI(s)).isSuccess
      case "BigInt"     => s.matches("\\d+")
      case "BigDecimal" => s.matches("\\d+(\\.\\d+)?")
      case _            => true // String, UUID, URI
    }

  protected def idBase(colIndex: Int): Int => String =
    (rowIndex: Int) => s"col-${colIndex + 1} row-${rowIndex + 1}"
}
