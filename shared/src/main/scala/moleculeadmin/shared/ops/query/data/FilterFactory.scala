package moleculeadmin.shared.ops.query.data
import java.util.regex.Pattern
import molecule.util.{DateHandling, RegexMatching}
import moleculeadmin.shared.ast.query.{Col, Filter}
import moleculeadmin.shared.util.PredicateMerger._


trait FilterFactory extends RegexMatching with DateHandling {


  def predNumber(token: String): Option[Double => Boolean] = token.trim match {
    case r"(-?\d+)$n"                 => Some(_ == n.toLong)
    case r"< *(-?\d+)$n"              => Some(_ < n.toLong)
    case r"> *(-?\d+)$n"              => Some(_ > n.toLong)
    case r"<= *(-?\d+)$n"             => Some(_ <= n.toLong)
    case r">= *(-?\d+)$n"             => Some(_ >= n.toLong)
    case r"(-?\d+)$n1 *- *(-?\d+)$n2" => Some(d => n1.toLong <= d && d <= n2.toLong)
    case ""                           => None
    case other                        =>
      println(s"Unrecognized number filter expr: `$other`")
      None
  }

  def predDecimal(token: String): Option[Double => Boolean] = token.trim match {
    case r"(-?\d+\.?\d*)$n"                       => Some(_ == n.toDouble)
    case r"< *(-?\d+\.?\d*)$n"                    => Some(_ < n.toDouble)
    case r"> *(-?\d+\.?\d*)$n"                    => Some(_ > n.toDouble)
    case r"<= *(-?\d+\.?\d*)$n"                   => Some(_ <= n.toDouble)
    case r">= *(-?\d+\.?\d*)$n"                   => Some(_ >= n.toDouble)
    case r"(-?\d+\.?\d*)$n1 *- *(-?\d+\.?\d*)$n2" => Some(d => n1.toDouble <= d && d <= n2.toDouble)
    case ""                                       => None
    case other                                    =>
      println(s"Unrecognized decimal filter expr: `$other`")
      None
  }

  def predBigInt(token: String): Option[String => Boolean] = token.trim match {
    case r"(-?\d+)$n"                 => Some(s => BigInt(s) == BigInt(n))
    case r"< *(-?\d+)$n"              => Some(s => BigInt(s) < BigInt(n))
    case r"> *(-?\d+)$n"              => Some(s => BigInt(s) > BigInt(n))
    case r"<= *(-?\d+)$n"             => Some(s => BigInt(s) <= BigInt(n))
    case r">= *(-?\d+)$n"             => Some(s => BigInt(s) >= BigInt(n))
    case r"(-?\d+)$n1 *- *(-?\d+)$n2" => Some(s => BigInt(n1) <= BigInt(s) && BigInt(s) <= BigInt(n2))
    case ""                           => None
    case other                        =>
      println(s"Unrecognized number filter expr: `$other`")
      None
  }

  def predBigDecimal(token: String): Option[String => Boolean] = token.trim match {
    case r"(-?\d+\.?\d*)$n"                       => Some(s => BigDecimal(s) == BigDecimal(n))
    case r"< *(-?\d+\.?\d*)$n"                    => Some(s => BigDecimal(s) < BigDecimal(n))
    case r"> *(-?\d+\.?\d*)$n"                    => Some(s => BigDecimal(s) > BigDecimal(n))
    case r"<= *(-?\d+\.?\d*)$n"                   => Some(s => BigDecimal(s) <= BigDecimal(n))
    case r">= *(-?\d+\.?\d*)$n"                   => Some(s => BigDecimal(s) >= BigDecimal(n))
    case r"(-?\d+\.?\d*)$n1 *- *(-?\d+\.?\d*)$n2" => Some(s => BigDecimal(n1) <= BigDecimal(s) && BigDecimal(s) <= BigDecimal(n2))
    case ""                                       => None
    case other                                    =>
      println(s"Unrecognized decimal filter expr: `$other`")
      None
  }

  def predDate(token: String): Option[String => Boolean] = {
    // Verify date conversions without throwing exceptions in the browser
    def verifiedDates(body: => String => Boolean): Option[String => Boolean] = try {
      Some {body}
    } catch {
      case e: Throwable =>
        println(e.getMessage)
        None
    }

    token.trim match {
      case r"(\d{1,4})$y"                                                                                                             => Some(_.startsWith(y))
      case r"(\d{1,4})$y *-? *(\d{1,2})$m"                                                                                            => Some(_.startsWith(s"$y-$m"))
      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d"                                                                           => Some(_.startsWith(s"$y-$m-$d"))
      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d *T? *(\d{1,2})$hh"                                                         => Some(_.startsWith(s"$y-$m-${d}T$hh"))
      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d *T? *(\d{1,2})$hh *:? *(\d{1,2})$mm"                                       => Some(_.startsWith(s"$y-$m-${d}T$hh:$mm"))
      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d *T? *(\d{1,2})$hh *:? *(\d{1,2})$mm *:? *(\d{1,2})$ss"                     => Some(_.startsWith(s"$y-$m-${d}T$hh:$mm:$ss"))
      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d *T? *(\d{1,2})$hh *:? *(\d{1,2})$mm *:? *(\d{1,2})$ss *\.? *(\d{1,3})$sss" => Some(_.startsWith(s"$y-$m-${d}T$hh:$mm:$ss.$sss"))

      case r"< *([0-9\-T: ]+)$d1" => verifiedDates {
        val t1 = str2date(d1).getTime
        d => str2date(d).getTime < t1
      }

      case r"> *([0-9\-T: ]+)$d1" => verifiedDates {
        val t1 = str2date(d1).getTime
        d => str2date(d).getTime > t1
      }

      case r"<= *([0-9\-T: ]+)$d1" => verifiedDates {
        val t1 = str2date(d1).getTime
        d => str2date(d).getTime <= t1
      }

      case r">= *([0-9\-T: ]+)$d1" => verifiedDates {
        val t1 = str2date(d1).getTime
        d => str2date(d).getTime >= t1
      }

      case r"([0-9\-T: ]+)$d1 *-- *([0-9\-T: ]+)$d2" => verifiedDates {
        val (t1, t2) = (str2date(d1).getTime, str2date(d2).getTime)
        d => {
          val t = str2date(d).getTime
          t1 <= t && t <= t2
        }
      }

      case ""    => None
      case other =>
        println(s"Unrecognized date filter expr: `$other`")
        None
    }
  }

  def predBoolean(token: String): Option[String => Boolean] = token.trim match {
    case "1"          => Some(_ == "true")
    case "0"          => Some(_ == "false")
    case r"tr?u?e?"   => Some(_ == "true")
    case r"fa?l?s?e?" => Some(_ == "false")
    case _            => None
  }

  def predString(token: String): Option[String => Boolean] = {
    token.trim match {
      case r"/(.*)$regex/?"   => Some(s => s.matches(regex))
      case r"i/(.*)$regex/?"  => Some(s =>
        Pattern.compile(regex, Pattern.CASE_INSENSITIVE).matcher(s).matches()
      )
      case r"!/(.*)$regex/?"  => Some(s =>
        Pattern.compile(s"(?!$regex)").matcher(s).matches()
      )
      case r"!i/(.*)$regex/?" => Some(s =>
        Pattern.compile(s"(?!$regex)", Pattern.CASE_INSENSITIVE).matcher(s).matches()
      )
      case ""                 => None
      case r"v *=>(.*)"       => None // todo?: compile Scala filter expr
      case anyStr             => Some(s => s.contains(anyStr))
    }
  }


  def predEid(
    s: Set[Long],
    f: Set[Long],
    c: Set[Long],
  )(token: String): Option[Double => Boolean] = token.trim match {
    case r"([sfcSFC]{1,3})$markers"   =>
      // check valid combination of
      // s - starred
      // f - flagged
      // c - checked
      // S - unstarred
      // F - unflagged
      // C - unchecked
      markers.length match {
        case 1 => markers match {
          case "s" => Some((d: Double) => s.contains(d.toLong))
          case "f" => Some((d: Double) => f.contains(d.toLong))
          case "c" => Some((d: Double) => c.contains(d.toLong))
          case "S" => Some((d: Double) => !s.contains(d.toLong))
          case "F" => Some((d: Double) => !f.contains(d.toLong))
          case "C" => Some((d: Double) => !c.contains(d.toLong))
        }
        case 2 => markers.toList.sorted match {
          case List('C', 'F') => Some((d: Double) => !c.contains(d.toLong) && !f.contains(d.toLong))
          case List('C', 'S') => Some((d: Double) => !c.contains(d.toLong) && !s.contains(d.toLong))
          case List('C', 'f') => Some((d: Double) => !c.contains(d.toLong) && f.contains(d.toLong))
          case List('C', 's') => Some((d: Double) => !c.contains(d.toLong) && s.contains(d.toLong))
          case List('F', 'S') => Some((d: Double) => !f.contains(d.toLong) && !s.contains(d.toLong))
          case List('F', 'c') => Some((d: Double) => !f.contains(d.toLong) && c.contains(d.toLong))
          case List('F', 's') => Some((d: Double) => !f.contains(d.toLong) && s.contains(d.toLong))
          case List('S', 'c') => Some((d: Double) => !s.contains(d.toLong) && c.contains(d.toLong))
          case List('S', 'f') => Some((d: Double) => !s.contains(d.toLong) && f.contains(d.toLong))
          case List('c', 'f') => Some((d: Double) => c.contains(d.toLong) && f.contains(d.toLong))
          case List('c', 's') => Some((d: Double) => c.contains(d.toLong) && s.contains(d.toLong))
          case List('f', 's') => Some((d: Double) => f.contains(d.toLong) && s.contains(d.toLong))
          case _              => println("Non-valid marker combination: " + markers); None
        }
        case 3 => markers.toList.sorted match {
          case List('C', 'F', 'S') => Some((d: Double) => !c.contains(d.toLong) && !f.contains(d.toLong) && !s.contains(d.toLong))
          case List('C', 'F', 's') => Some((d: Double) => !c.contains(d.toLong) && !f.contains(d.toLong) && s.contains(d.toLong))
          case List('C', 'S', 'f') => Some((d: Double) => !c.contains(d.toLong) && !s.contains(d.toLong) && f.contains(d.toLong))
          case List('C', 'f', 's') => Some((d: Double) => !c.contains(d.toLong) && f.contains(d.toLong) && s.contains(d.toLong))
          case List('F', 'S', 'c') => Some((d: Double) => !f.contains(d.toLong) && !s.contains(d.toLong) && c.contains(d.toLong))
          case List('F', 'c', 's') => Some((d: Double) => !f.contains(d.toLong) && c.contains(d.toLong) && s.contains(d.toLong))
          case List('S', 'c', 'f') => Some((d: Double) => !s.contains(d.toLong) && c.contains(d.toLong) && f.contains(d.toLong))
          case List('c', 'f', 's') => Some((d: Double) => c.contains(d.toLong) && f.contains(d.toLong) && s.contains(d.toLong))
          case _                   => println("Non-valid marker combination: " + markers); None
        }
      }
    case r"(-?\d+)$n"                 => Some(_ == n.toLong)
    case r"< *(-?\d+)$n"              => Some(_ < n.toLong)
    case r"> *(-?\d+)$n"              => Some(_ > n.toLong)
    case r"<= *(-?\d+)$n"             => Some(_ <= n.toLong)
    case r">= *(-?\d+)$n"             => Some(_ >= n.toLong)
    case r"(-?\d+)$n1 *- *(-?\d+)$n2" => Some(d => n1.toLong <= d && d <= n2.toLong)
    case ""                           => None
    case other                        => println(s"Unrecognized entity id filter expr: `$other`"); None
  }

  def mergedPredicate[T](
    filterExpr: String,
    predicateFactory: String => Option[T => Boolean],
    splitComma: Boolean
  ): Option[T => Boolean] = {
    val predicates: Seq[T => Boolean] = if (splitComma)
      (for {
        lines <- filterExpr.split('\n')
        token <- lines.split(',')
        predicate <- predicateFactory(token)
      } yield predicate).toList
    else
      (for {
        token <- filterExpr.split('\n')
        predicate <- predicateFactory(token)
      } yield predicate).toList

    predicates.length match {
      case 0 => None
      case 1 => Some(predicates.head)
      case _ =>
        // merge multiple predicates with OR logic
        Some(predicates.tail.foldLeft(predicates.head)(_ or _))
    }
  }


  def createFilter(
    col: Col,
    filterExpr: String,
    curStars: Set[Long] = Set.empty[Long],
    curFlags: Set[Long] = Set.empty[Long],
    curChecks: Set[Long] = Set.empty[Long],
    splitComma: Boolean = true
  ): Option[Filter[_]] = {
    val Col(colIndex, _, _, _, _, attrType,
    colType, _, _, _, aggrType, _, _, _, _) = col

    def filter[T](
      predicateFactory: String => Option[T => Boolean]
    ): Option[Filter[T]] = {
      mergedPredicate(filterExpr, predicateFactory, splitComma) match {
        case None             => Option.empty[Filter[T]]
        case Some(mergedPred) => Some(
          Filter[T](colIndex, colType, filterExpr, mergedPred)
        )
      }
    }

    if (aggrType.isEmpty) {
      colType match {
        case "string" | "listString" => attrType match {
          case "Date"       => filter[String](predDate)
          case "Boolean"    => filter[String](predBoolean)
          case "BigInt"     => filter[String](predBigInt)
          case "BigDecimal" => filter[String](predBigDecimal)
          case _            => filter[String](predString)
        }
        case "double" | "listDouble" => attrType match {
          case "datom"                =>
            filter[Double](predEid(curStars, curFlags, curChecks))
          case "Int" | "Long" | "ref" => filter[Double](predNumber)
          case _                      => filter[Double](predDecimal)
        }
        case _                       => None
      }
    } else {
      aggrType match {
        case "aggrInt"    => filter[Double](predNumber)
        case "aggrDouble" => filter[Double](predDecimal)
        case _            => colType match {
          case "string" | "listString" => attrType match {
            case "Date"       => filter[String](predDate)
            case "Boolean"    => filter[String](predBoolean)
            case "BigInt"     => filter[String](predBigInt)
            case "BigDecimal" => filter[String](predBigDecimal)
            case _            => filter[String](predString)
          }
          case _                       => attrType match {
            case "datom"                =>
              filter[Double](predEid(curStars, curFlags, curChecks))
            case "Int" | "Long" | "ref" => filter[Double](predNumber)
            case _                      => filter[Double](predDecimal)
          }
        }
      }
    }
  }
}
