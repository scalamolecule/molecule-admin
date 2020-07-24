package moleculeadmin.client.app.logic.query.data

import java.net.URI
import java.util.UUID
import moleculeadmin.shared.util.HelpersAdmin
import scala.util.Try

trait TypeValidation extends HelpersAdmin {

  protected def valid(attrType: String, s: String): Boolean = if (s.isEmpty)
    true
  else
    attrType match {
      case "String"     => true
      case "Int"        => s.isInt
      case "ref"        =>
        // Allow test ref ids from 10000 and up.
        // Otherwise they can collide with attribute definition entity ids
        s.length > 4 && s.isLong
      case "Long"       => s.isLong
      case "Float"      => s.isFloat
      case "Double"     => s.isDouble
      case "Boolean"    => s.isBoolean
      case "Date"       => Try(expandDateStr(s)).isSuccess
      case "UUID"       => Try(UUID.fromString(s)).isSuccess
      case "URI"        => Try(new URI(s)).isSuccess
      case "BigInt"     => s.matches("\\d+")
      case "BigDecimal" => s.matches("\\d+(\\.\\d+)?")
    }
}
