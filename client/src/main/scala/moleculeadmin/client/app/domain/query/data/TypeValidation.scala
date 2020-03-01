package moleculeadmin.client.app.domain.query.data

import java.net.URI
import java.util.UUID
import moleculeadmin.shared.util.HelpersAdmin
import scala.util.Try

trait TypeValidation extends HelpersAdmin {

  protected def valid(attrType: String, s: String): Boolean = if (s.isEmpty)
    true
  else
    attrType match {
      case "String"       => true
      case "Int"          => s.isInt
      case "Long" | "ref" => s.isLong
      case "Float"        => s.isFloat
      case "Double"       => s.isDouble
      case "Boolean"      => s.isBoolean
      case "Date"         => Try(expandDateStr(s)).isSuccess
      case "UUID"         => Try(UUID.fromString(s)).isSuccess
      case "URI"          => Try(new URI(s)).isSuccess
      case "BigInt"       => s.matches("\\d+")
      case "BigDecimal"   => s.matches("\\d+(\\.\\d+)?")
    }
}
