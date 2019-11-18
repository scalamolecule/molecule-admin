package moleculeadmin.client.app.domain.query.data.groupedit.ops
import moleculeadmin.shared.util.HelpersAdmin


trait TypeMappings extends HelpersAdmin {

  def getTypeMappings(attr: String, tpe: String, card: Int)
  : (String, String) = {
    val cleanAttr = clean(attr)
    val opt       = attr.last == '$'
    card match {
      case 1 if opt =>
        // Need to assign new Option to satisfy JS compiler
        tpe match {
          case "Int"                               => (
            s"Option[Int]",
            s"$attr.toOption.fold(Option.empty[Int])(v => Some(v.toInt))"
          )
          case "Long" | "datom" | "ref" | "BigInt" => (
            "Option[BigInt]",
            s"$attr.toOption.fold(Option.empty[BigInt])(v => Some(BigInt(v)))"
          )
          case "Float" | "Double" | "BigDecimal"   => (
            "Option[BigDecimal]",
            s"$attr.toOption.fold(Option.empty[BigDecimal])(v => Some(BigDecimal(v)))"
          )
          case "String"                            => (
            s"Option[String]",
            s"$attr.toOption.fold(Option.empty[String])(v => Some(v))"
          )
          case "Boolean"                           => (
            s"Option[Boolean]",
            s"$attr.toOption.fold(Option.empty[Boolean])(v => Some(v.toBoolean))"
          )
          case "Date"                              => (
            s"Option[LocalDateTime]",
            s"$attr.toOption.fold(Option.empty[LocalDateTime])(d => Some(str2ldt(d)))"
          )
          case "UUID"                              => (
            s"Option[UUID]",
            s"$attr.toOption.fold(Option.empty[UUID])(v => Some(UUID.fromString(v)))"
          )
          case "URI"                               => (
            s"Option[URI]",
            s"$attr.toOption.fold(Option.empty[URI])(v => Some(new URI(v)))"
          )
        }

      case 1 =>
        tpe match {
          case "Int"                               => ("Int", s"$attr.toInt")
          case "Long" | "datom" | "ref" | "BigInt" => ("BigInt", s"BigInt($attr)")
          case "Float" | "Double" | "BigDecimal"   => ("BigDecimal", s"BigDecimal($attr)")
          case "String"                            => ("String", s"$attr")
          case "Boolean"                           => ("Boolean", s"$attr.toBoolean")
          case "Date"                              => ("LocalDateTime", s"str2ldt($attr)")
          case "UUID"                              => ("UUID", s"UUID.fromString($attr)")
          case "URI"                               => ("URI", s"new URI($attr)")
        }

      case 2 =>
        tpe match {
          case "Int"                               => (
            s"List[Int]",
            s"$cleanAttr.toList.map(_.toInt)"
          )
          case "Long" | "datom" | "ref" | "BigInt" => (
            "List[BigInt]",
            s"$cleanAttr.toList.map(BigInt(_))"
          )
          case "Float" | "Double" | "BigDecimal"   => (
            "List[BigDecimal]",
            s"$cleanAttr.toList.map(BigDecimal(_))"
          )
          case "String"                            => (
            s"List[String]",
            s"$cleanAttr.toList"
          )
          case "Date"                              => (
            "List[LocalDateTime]",
            s"$cleanAttr.toList.map(str2ldt(_))"
          )
          case "Boolean"                           => (
            s"List[Boolean]",
            s"$cleanAttr.toList.map(_.toBoolean)"
          )
          case "UUID"                              => (
            s"List[UUID]",
            s"$cleanAttr.toList.map(UUID.fromString(_))"
          )
          case "URI"                               => (
            s"List[URI]",
            s"$cleanAttr.toList.map(new URI(_))"
          )
        }

      case 3 =>
        tpe match {
          case "Int"                               => (
            s"Map[String, Int]",
            s"$attr.toMap.map { case (k, v) => k -> v.toInt }"
          )
          case "Long" | "datom" | "ref" | "BigInt" => (
            "Map[String, BigInt]",
            s"$attr.toMap.map { case (k, v) => k -> BigInt(v) }"
          )
          case "Float" | "Double" | "BigDecimal"   => (
            "Map[String, BigDecimal]",
            s"$attr.toMap.map { case (k, v) => k -> BigDecimal(v) }"
          )
          case "String"                            => (
            s"Map[String, String]",
            s"$attr.toMap"
          )
          case "Date"                              => (
            "Map[String, LocalDateTime]",
            s"$attr.toMap.map { case (k, v) => k -> str2ldt(v) }"
          )
          case "Boolean"                           => (
            s"Map[String, Boolean]",
            s"$attr.toMap.map { case (k, v) => k -> v.toBoolean }"
          )
          case "UUID"                              => (
            s"Map[String, UUID]",
            s"$attr.toMap.map { case (k, v) => k -> UUID.fromString(v) }"
          )
          case "URI"                               => (
            s"Map[String, URI]",
            s"$attr.toMap.map { case (k, v) => k -> new URI(v) }"
          )
        }
    }
  }
}
