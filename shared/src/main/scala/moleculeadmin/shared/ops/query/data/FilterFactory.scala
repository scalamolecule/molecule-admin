package moleculeadmin.shared.ops.query.data
import moleculeadmin.shared.ast.query.{Col, Filter}
import moleculeadmin.shared.util.PredicateMerger._
import moleculeadmin.shared.util.{DateHandling, RegexMatching}
import java.util.regex.Pattern


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
    val regexNoCase = Pattern.compile(".*", Pattern.CASE_INSENSITIVE)
    val negRegEx = Pattern.compile("(?!.*)")
    val negRegExNoCase = Pattern.compile("(?!.*)", Pattern.CASE_INSENSITIVE)
    token.trim match {
      case r"/(.*)$regex/?"   => Some(s => s.matches(regex))
      case r"i/(.*)$regex/?"  => Some(s => regexNoCase.matcher(s).matches())
      case r"!/(.*)$regex/?"  => Some(s => negRegEx.matcher(s).matches())
      case r"!i/(.*)$regex/?" => Some(s => negRegExNoCase.matcher(s).matches())
      case ""                 => None
      case r"v *=>(.*)"       => None // todo: compile Scala filter expr
      case anyStr             => Some(s => s.contains(anyStr))
    }
  }

  def mergedPredicate[T](filterExpr: String,
                         predicateFactory: String => Option[T => Boolean]
                        ): Option[T => Boolean] = {
    val fns: Seq[T => Boolean] = (for {
      lines <- filterExpr.split('\n')
      token <- lines.split(',')
      fn <- predicateFactory(token)
    } yield fn).toList

    fns.length match {
      case 0 => None
      case 1 => Some(fns.head)
      // merge multiple predicates with OR logic
      // (uses implicit in imported PredicateMerger)
      case _ => Some(fns.tail.foldLeft(fns.head)(_ or _))
    }
  }


  //  private
  def filter[T](colIndex: Int,
                colType: String,
                filterExpr: String,
                predicateFactory: String => Option[T => Boolean]
               ): Option[Filter[T]] = {
    mergedPredicate(filterExpr, predicateFactory) match {
      case None       => Option.empty[Filter[T]]
      case Some(pred) => Some(Filter[T](colIndex, colType, filterExpr, pred))
    }
  }


  def createFilter(col: Col, filterExpr: String): Option[Filter[_]] = {
    val Col(colIndex, _, _, _, _, attrType, colType, _, _, _, aggrType, _, _, _) = col
    if (aggrType.isEmpty) {
      colType match {
        case "string" | "listString" => attrType match {
          case "Date"       => filter[String](colIndex, colType, filterExpr, predDate)
          case "Boolean"    => filter[String](colIndex, colType, filterExpr, predBoolean)
          case "BigInt"     => filter[String](colIndex, colType, filterExpr, predBigInt)
          case "BigDecimal" => filter[String](colIndex, colType, filterExpr, predBigDecimal)
          case _            => filter[String](colIndex, colType, filterExpr, predString)
        }
        case "double" | "listDouble" => attrType match {
          case "Int" | "Long" | "ref" => filter[Double](colIndex, colType, filterExpr, predNumber)
          case _                      => filter[Double](colIndex, colType, filterExpr, predDecimal)
        }
        case _                       => None
      }
    } else {
      aggrType match {
        case "aggrInt"    => filter[Double](colIndex, colType, filterExpr, predNumber)
        case "aggrDouble" => filter[Double](colIndex, colType, filterExpr, predDecimal)
        case _            => colType match {
          case "string" | "listString" => attrType match {
            case "Date"       => filter[String](colIndex, colType, filterExpr, predDate)
            case "Boolean"    => filter[String](colIndex, colType, filterExpr, predBoolean)
            case "BigInt"     => filter[String](colIndex, colType, filterExpr, predBigInt)
            case "BigDecimal" => filter[String](colIndex, colType, filterExpr, predBigDecimal)
            case _            => filter[String](colIndex, colType, filterExpr, predString)
          }
          case _                       => attrType match {
            case "Int" | "Long" | "ref" | "datom" => filter[Double](colIndex, colType, filterExpr, predNumber)
            case _                                => filter[Double](colIndex, colType, filterExpr, predDecimal)
          }
        }
      }
    }
  }
}
