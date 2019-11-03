package moleculeadmin.client.app.domain.query.data.groupedit.ops
import moleculeadmin.shared.util.HelpersAdmin


trait TypeMappings extends HelpersAdmin {

  def getTypeMappings(attr: String, tpe: String, card: Int)
  : (String, String, String) = {
    val opt = attr.last == '$'
    card match {
      case 1 if opt =>
        // Need to assign new Option to satisfy JS compiler
        tpe match {
          case "Long" | "datom" | "ref" | "BigInt" => (
            "js.UndefOr[String]",
            "Option[BigInt]",
            s"$attr.toOption.fold(Option.empty[BigInt])(v => Some(BigInt(v)))"
          )

          case "Float" | "Double" | "BigDecimal" => (
            "js.UndefOr[String]",
            "Option[BigDecimal]",
            s"$attr.toOption.fold(Option.empty[BigDecimal])(v => Some(BigDecimal(v)))"
          )

          case "Date" => (
            "js.UndefOr[LocalDateTime]",
            s"Option[LocalDateTime]",
            s"$attr.toOption.fold(Option.empty[LocalDateTime])(d => Some(cloneDate(d)))"
          )

          // Int + String types
          case _ => (
            s"js.UndefOr[$tpe]",
            s"Option[$tpe]",
            s"$attr.toOption.fold(Option.empty[$tpe])(v => Some(v))"
          )
        }

      case 1 =>
        tpe match {
          case "Long" | "datom" | "ref" | "BigInt" =>
            ("String", "BigInt", s"BigInt($attr)")

          case "Float" | "Double" | "BigDecimal" =>
            ("String", "BigDecimal", s"BigDecimal($attr)")

          case "Date" =>
            ("LocalDateTime", "LocalDateTime", s"cloneDate($attr)")

          case _ =>
            (tpe, tpe, s"$attr")
        }

      case 2 =>
        tpe match {
          case "Long" | "datom" | "ref" | "BigInt" => (
            "js.Array[String]",
            "List[BigInt]",
            s"${clean(attr)}.toList.map(v => BigInt(v))"
          )
          case "Float" | "Double" | "BigDecimal"   => (
            "js.Array[String]",
            "List[BigDecimal]",
            s"${clean(attr)}.toList.map(v => BigDecimal(v))"
          )
          case "Date"                              => (
            "js.Array[LocalDateTime]",
            "List[LocalDateTime]",
            s"${clean(attr)}.toList.map(v => cloneDate(v))"
          )
          case _                                   => (
            s"js.Array[$tpe]",
            s"List[$tpe]",
            s"${clean(attr)}.toList"
          )
        }
      //
      //        case 2 =>
      //          tpe match {
      //            case "Int"                               => (
      //              "js.Array[Int]",
      //              "js.Array[Int]",
      //              attr
      //            )
      //            case "Long" | "datom" | "ref" | "BigInt" => (
      //              "js.Array[BigInt]",
      //              "js.Array[String]",
      //              s"$attr.map(v => BigInt(v))"
      //            )
      //            case "Float" | "Double" | "BigDecimal"   => (
      //              "js.Array[BigDecimal]",
      //              "js.Array[String]",
      //              s"$attr.map(v => BigDecimal(v))"
      //            )
      //            case "Date"                              => (
      //              "js.Array[js.Date]",
      //              "js.Array[js.Date]",
      //              attr
      //            )
      //            case _                                   => (
      //              s"js.Array[$tpe]",
      //              s"js.Array[$tpe]",
      //              attr
      //            )
      //          }
      //
      //    case 3 =>
      //      tpe match {
      //        case "Int"                               => (
      //          "js.Dictionary[Int]",
      //          "Map[String, Int]",
      //          s"$attr.toJSDictionary"
      //        )
      //        case "Long" | "datom" | "ref" | "BigInt" => (
      //          "js.Dictionary[String]",
      //          "Map[String, String]",
      //          s"$attr.toJSDictionary.map { case (k, v) => k -> BigInt(v) }"
      //        )
      //        case "Float" | "Double" | "BigDecimal"   => (
      //          "js.Dictionary[String]",
      //          "Map[String, String]",
      //          s"$attr.toJSDictionary.map { case (k, v) => k -> BigDecimal(v) }"
      //        )
      //        case "Date"                              => (
      //          "js.Dictionary[js.Date]",
      //          "Map[String, js.Date]",
      //          s"$attr.toJSDictionary"
      //        )
      //        case _                                   => (
      //          s"js.Dictionary[$tpe]",
      //          s"Map[String, $tpe]",
      //          s"$attr.toJSDictionary",
      //        )
      //      }
    }
  }

  def getTypeMappingsX(attr: String, tpe: String, card: Int)
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
