package moleculeadmin.client.app.domain.query.data.groupedit.ops


trait TypeMappings {

  def getTypeMappings(attr: String, tpe: String, card: Int)
  : (String, String, String) = card match {
    case 1 if attr.last == '$' =>
      tpe match {
        case "Long" | "datom" | "ref" | "BigInt" => (
          "Option[String)",
          "Option[String)",
          s"$attr.fold(Option.empty[BigInt])(v => Some(BigInt(v))"
        )

        case "Float" | "Double" | "BigDecimal" => (
          "Option[String]",
          "Option[String]",
          s"$attr.fold(Option.empty[BigDecimal])(v => Some(BigDecimal(v))"
        )

        case "Date" => (
          s"Option[LocalDateTime]",
          s"Option[LocalDateTime]",
          attr
        )

        // Int + String types
        case _ => (
          s"Option[$tpe]",
          s"Option[$tpe]",
          attr
        )
      }

    case 1 =>
      tpe match {
        case "Long" | "datom" | "ref" | "BigInt" =>
          ("BigInt", "String", s"BigInt($attr)")

        case "Float" | "Double" | "BigDecimal" =>
          ("BigDecimal", "String", s"BigDecimal($attr)")

        case "Date" =>
          ("LocalDateTime", "LocalDateTime", attr)

        case _ =>
          (tpe, tpe, attr)

      }

    case 2 =>
      tpe match {
        case "Int"                               => (
          "js.Array[Int]",
          "js.Array[Int]",
          attr
        )
        case "Long" | "datom" | "ref" | "BigInt" => (
          "js.Array[BigInt]",
          "js.Array[String]",
          s"$attr.map(v => BigInt(v))"
        )
        case "Float" | "Double" | "BigDecimal"   => (
          "js.Array[BigDecimal]",
          "js.Array[String]",
          s"$attr.map(v => BigDecimal(v))"
        )
        case "Date"                              => (
          "js.Array[js.Date]",
          "js.Array[js.Date]",
          attr
        )
        case _                                   => (
          s"js.Array[$tpe]",
          s"js.Array[$tpe]",
          attr
        )
      }

    case 3 =>
      tpe match {
        case "Int"                               => (
          "js.Dictionary[Int]",
          "Map[String, Int]",
          s"$attr.toJSDictionary"
        )
        case "Long" | "datom" | "ref" | "BigInt" => (
          "js.Dictionary[String]",
          "Map[String, String]",
          s"$attr.toJSDictionary.map { case (k, v) => k -> BigInt(v) }"
        )
        case "Float" | "Double" | "BigDecimal"   => (
          "js.Dictionary[String]",
          "Map[String, String]",
          s"$attr.toJSDictionary.map { case (k, v) => k -> BigDecimal(v) }"
        )
        case "Date"                              => (
          "js.Dictionary[js.Date]",
          "Map[String, js.Date]",
          s"$attr.toJSDictionary"
        )
        case _                                   => (
          s"js.Dictionary[$tpe]",
          s"Map[String, $tpe]",
          s"$attr.toJSDictionary",
        )
      }
  }
}
