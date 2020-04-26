package moleculeadmin.shared.ops.query.data

import java.util.regex.Pattern
import molecule.util.{DateHandling, RegexMatching}
import moleculeadmin.shared.ast.query._
import moleculeadmin.shared.util.PredicateMerger._
import scala.language.existentials

trait FilterFactory extends RegexMatching with DateHandling {

  def createFilter(
    col: Col,
    filterExpr: String,
    splitComma: Boolean = true
  ): Option[Filter[_]] = {
    val Col(colIndex, _, _, _, _, attrType,
    colType, _, _, _, aggrType, _, _, _, _) = col

    def filter[T](
      predicateFactory: String => Option[Markers => Option[T] => Boolean]
    ): Option[Filter[T]] = {
      mergedPredicate(filterExpr, predicateFactory, splitComma) match {
        case None             =>
          Option.empty[Filter[T]]
        case Some(mergedPred) =>
          Some(
            Filter[T](colIndex, colType, aggrType.nonEmpty, filterExpr, mergedPred)
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
          case "datom"                => filter[Double](predEid)
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
            case "datom"                => filter[Double](predEid)
            case "Int" | "Long" | "ref" => filter[Double](predNumber)
            case _                      => filter[Double](predDecimal)
          }
        }
      }
    }
  }


  def mergedPredicate[T](
    filterExpr: String,
    predicateFactory: String => Option[Markers => Option[T] => Boolean],
    splitComma: Boolean
  ): Option[Markers => Option[T] => Boolean] = {
    val predicates: Seq[Markers => Option[T] => Boolean] =
      filterExpr match {
        case r"!?i?/.*" =>
          // Ignore line shifts in regex expressions to allow overviewing complex
          // expressions on multiple lines without affecting regex execution
          Seq(predicateFactory(filterExpr.replaceAll("\n", "")).get)

        case expr if splitComma =>
          (for {
            lines <- expr.split('\n')
            token <- lines.split(',')
            predicate <- predicateFactory(token)
          } yield predicate).toList

        case expr =>
          (for {
            token <- expr.split('\n')
            predicate <- predicateFactory(token)
          } yield predicate).toList
      }

    predicates.length match {
      case 0 => None
      case 1 => Some(predicates.head)
      case _ => Some(predicates.tail.foldLeft(predicates.head)(_ or _))
    }
  }

  def mergedPredicateXX[T](
    filterExpr: String,
    predicateFactory: String => Option[Option[T] => Boolean],
    splitComma: Boolean
  ): Option[Option[T] => Boolean] = {
    val predicates: Seq[Option[T] => Boolean] =
      filterExpr match {
        case r"!?i?/.*" =>
          // Ignore line shifts in regex expressions to allow overviewing complex
          // expressions on multiple lines without affecting regex execution
          Seq(predicateFactory(filterExpr.replaceAll("\n", "")).get)

        case expr if splitComma =>
          (for {
            lines <- expr.split('\n')
            token <- lines.split(',')
            predicate <- predicateFactory(token)
          } yield predicate).toList

        case expr =>
          (for {
            token <- expr.split('\n')
            predicate <- predicateFactory(token)
          } yield predicate).toList
      }

    predicates.length match {
      case 0 => None
      case 1 => Some(predicates.head)
      case _ => Some(predicates.tail.foldLeft(predicates.head)(_ or _))
    }
  }

  def predEid(token: String): Option[Markers => Option[Double] => Boolean] = {
    token.trim match {
      case ""                         => None
      case r"([sfcSFC]{1,3})$markers" =>
        // check valid combination of
        // s - starred
        // f - flagged
        // c - checked
        // S - unstarred
        // F - unflagged
        // C - unchecked
        markers.length match {
          case 1 => markers match {
            case "s" => Some(m => _.fold(false)((d: Double) => m._1.contains(d.toLong)))
            case "f" => Some(m => _.fold(false)((d: Double) => m._2.contains(d.toLong)))
            case "c" => Some(m => _.fold(false)((d: Double) => m._3.contains(d.toLong)))
            case "S" => Some(m => _.fold(false)((d: Double) => !m._1.contains(d.toLong)))
            case "F" => Some(m => _.fold(false)((d: Double) => !m._2.contains(d.toLong)))
            case "C" => Some(m => _.fold(false)((d: Double) => !m._3.contains(d.toLong)))
          }

          case 2 => markers.toList.sorted match {
            case List('C', 'F') => Some(m => _.fold(false)((d: Double) => !m._3.contains(d.toLong) && !m._2.contains(d.toLong)))
            case List('C', 'S') => Some(m => _.fold(false)((d: Double) => !m._3.contains(d.toLong) && !m._1.contains(d.toLong)))
            case List('C', 'f') => Some(m => _.fold(false)((d: Double) => !m._3.contains(d.toLong) && m._2.contains(d.toLong)))
            case List('C', 's') => Some(m => _.fold(false)((d: Double) => !m._3.contains(d.toLong) && m._1.contains(d.toLong)))
            case List('F', 'S') => Some(m => _.fold(false)((d: Double) => !m._2.contains(d.toLong) && !m._1.contains(d.toLong)))
            case List('F', 'c') => Some(m => _.fold(false)((d: Double) => !m._2.contains(d.toLong) && m._3.contains(d.toLong)))
            case List('F', 's') => Some(m => _.fold(false)((d: Double) => !m._2.contains(d.toLong) && m._1.contains(d.toLong)))
            case List('S', 'c') => Some(m => _.fold(false)((d: Double) => !m._1.contains(d.toLong) && m._3.contains(d.toLong)))
            case List('S', 'f') => Some(m => _.fold(false)((d: Double) => !m._1.contains(d.toLong) && m._2.contains(d.toLong)))
            case List('c', 'f') => Some(m => _.fold(false)((d: Double) => m._3.contains(d.toLong) && m._2.contains(d.toLong)))
            case List('c', 's') => Some(m => _.fold(false)((d: Double) => m._3.contains(d.toLong) && m._1.contains(d.toLong)))
            case List('f', 's') => Some(m => _.fold(false)((d: Double) => m._2.contains(d.toLong) && m._1.contains(d.toLong)))
            case _              => println("Non-valid marker combination: " + markers); None
          }

          case 3 => markers.toList.sorted match {
            case List('C', 'F', 'S') => Some(m => _.fold(false)((d: Double) => !m._3.contains(d.toLong) && !m._2.contains(d.toLong) && !m._1.contains(d.toLong)))
            case List('C', 'F', 's') => Some(m => _.fold(false)((d: Double) => !m._3.contains(d.toLong) && !m._2.contains(d.toLong) && m._1.contains(d.toLong)))
            case List('C', 'S', 'f') => Some(m => _.fold(false)((d: Double) => !m._3.contains(d.toLong) && !m._1.contains(d.toLong) && m._2.contains(d.toLong)))
            case List('C', 'f', 's') => Some(m => _.fold(false)((d: Double) => !m._3.contains(d.toLong) && m._2.contains(d.toLong) && m._1.contains(d.toLong)))
            case List('F', 'S', 'c') => Some(m => _.fold(false)((d: Double) => !m._2.contains(d.toLong) && !m._1.contains(d.toLong) && m._3.contains(d.toLong)))
            case List('F', 'c', 's') => Some(m => _.fold(false)((d: Double) => !m._2.contains(d.toLong) && m._3.contains(d.toLong) && m._1.contains(d.toLong)))
            case List('S', 'c', 'f') => Some(m => _.fold(false)((d: Double) => !m._1.contains(d.toLong) && m._3.contains(d.toLong) && m._2.contains(d.toLong)))
            case List('c', 'f', 's') => Some(m => _.fold(false)((d: Double) => m._3.contains(d.toLong) && m._2.contains(d.toLong) && m._1.contains(d.toLong)))
            case _                   => println("Non-valid marker combination: " + markers); None
          }
        }

      case r"(-?\d+)$n"                 => Some(_ => _.fold(false)(_ == n.toLong))
      case r"<= *(-?\d+)$n"             => Some(_ => _.fold(false)(_ <= n.toLong))
      case r">= *(-?\d+)$n"             => Some(_ => _.fold(false)(_ >= n.toLong))
      case r"< *(-?\d+)$n"              => Some(_ => _.fold(false)(_ < n.toLong))
      case r"> *(-?\d+)$n"              => Some(_ => _.fold(false)(_ > n.toLong))
      case r"(-?\d+)$n1 *- *(-?\d+)$n2" => Some(_ => _.fold(false)(d => n1.toLong <= d && d <= n2.toLong))
      case other                        => println(s"Unrecognized entity id filter expr: `$other`"); None
    }
  }


  def predString(token: String): Option[Markers => Option[String] => Boolean] = {
    // (don't trim to allow matching empty string)
    token match {
      case ""                 => None // no filter
      case "-"                => Some(_ => _.isEmpty)
      case "+"                => Some(_ => _.isDefined)
      case r"\{( *)$spaces\}" => Some(_ => _.fold(false)(s => s.matches(spaces)))
      case r"/(.*)$regex"     => Some(_ => _.fold(false)(s => s.matches(regex)))
      case r"i/(.*)$regex"    => Some(_ => _.fold(false)(s =>
        Pattern.compile(regex, Pattern.CASE_INSENSITIVE).matcher(s).matches()
      ))
      case r"!/(.*)$regex"    => Some(_ => _.fold(false) { s =>
        !Pattern.compile(regex).matcher(s).matches()
      })
      case r"!i/(.*)$regex"   => Some(_ => _.fold(false)(s =>
        !Pattern.compile(regex, Pattern.CASE_INSENSITIVE).matcher(s).matches()
      ))
      case r"v *=>(.*)"       => None // todo?: compile Scala filter expr
      case r"!(.*)$needle"    => Some(_ => _.fold(false)(s => !s.contains(needle)))
      case r">=(.*)$needle"   => Some(_ => _.fold(false)(s => s >= needle))
      case r">(.*)$needle"    => Some(_ => _.fold(false)(s => s > needle))
      case r"<=(.*)$needle"   => Some(_ => _.fold(false)(s => s <= needle))
      case r"<(.*)$needle"    => Some(_ => _.fold(false)(s => s < needle))
      case needle             => Some(_ => _.fold(false)(s => s.contains(needle)))
    }
  }

  def predDate(token: String): Option[Markers => Option[String] => Boolean] = {
    // Verify date conversions without throwing exceptions in the browser
    def verifiedDates(body: => Markers => Option[String] => Boolean): Option[Markers => Option[String] => Boolean] = try {
      Some {
        body
      }
    } catch {
      case e: Throwable =>
        println(e.getMessage)
        None
    }

    token.trim match {
      case ""  => None
      case "-" => Some(_ => _.isEmpty)
      case "+" => Some(_ => _.isDefined)

      case r"(\d{1,4})$y" =>
        Some(_ => _.fold(false)(_.startsWith(y)))

      case r"(\d{1,4})$y *-? *(\d{1,2})$m" =>
        Some(_ => _.fold(false)(_.startsWith(s"$y-$m")))

      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d" =>
        Some(_ => _.fold(false)(_.startsWith(s"$y-$m-$d")))

      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d *T? *(\d{1,2})$hh" =>
        Some(_ => _.fold(false)(_.startsWith(s"$y-$m-$d $hh")))

      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d *T? *(\d{1,2})$hh *:? *(\d{1,2})$mm" =>
        Some(_ => _.fold(false)(_.startsWith(s"$y-$m-$d $hh:$mm")))

      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d *T? *(\d{1,2})$hh *:? *(\d{1,2})$mm *:? *(\d{1,2})$ss" =>
        Some(_ => _.fold(false)(_.startsWith(s"$y-$m-$d $hh:$mm:$ss")))

      case r"(\d{1,4})$y *-? *(\d{1,2})$m *-? *(\d{1,2})$d *T? *(\d{1,2})$hh *:? *(\d{1,2})$mm *:? *(\d{1,2})$ss *\.? *(\d{1,3})$sss" =>
        Some(_ => _.fold(false)(_.startsWith(s"$y-$m-$d $hh:$mm:$ss.$sss")))

      case r"<= *([0-9\-T:\. ]+)$d1" => verifiedDates {
        val t1 = str2date(d1).getTime
        _ => _.fold(false)(d => str2date(d).getTime <= t1)
      }

      case r">= *([0-9\-T:\. ]+)$d1" => verifiedDates {
        val t1 = str2date(d1).getTime
        _ => _.fold(false)(d => str2date(d).getTime >= t1)
      }

      case r"< *([0-9\-T:\. ]+)$d1" => verifiedDates {
        val t1 = str2date(d1).getTime
        _ => _.fold(false)(d => str2date(d).getTime < t1)
      }

      case r"> *([0-9\-T:\. ]+)$d1" => verifiedDates {
        val t1 = str2date(d1).getTime
        _ => _.fold(false)(d => str2date(d).getTime > t1)
      }

      case r"([0-9\-T:\. ]+)$d1 *-- *([0-9\-T:\. ]+)$d2" => verifiedDates {
        val (t1, t2) = (str2date(d1).getTime, str2date(d2).getTime)
        _ =>
          _.fold(false)(d => {
            val t = str2date(d).getTime
            t1 <= t && t <= t2
          })
      }

      case other =>
        println(s"Unrecognized date filter expr: `$other`")
        None
    }
  }

  def predBoolean(token: String): Option[Markers => Option[String] => Boolean] = token.trim match {
    case ""           => None
    case "-"          => Some(_ => _.isEmpty)
    case "+"          => Some(_ => _.isDefined)
    case "1"          => Some(_ => _.fold(false)(_ == "true"))
    case "0"          => Some(_ => _.fold(false)(_ == "false"))
    case r"tr?u?e?"   => Some(_ => _.fold(false)(_ == "true"))
    case r"fa?l?s?e?" => Some(_ => _.fold(false)(_ == "false"))
    case other        =>
      println(s"Unrecognized boolean filter expr: `$other`")
      None
  }


  def predNumber(token: String): Option[Markers => Option[Double] => Boolean] = token.trim match {
    case ""                           => None
    case "-"                          => Some(_ => _.isEmpty)
    case "+"                          => Some(_ => _.isDefined)
    case r"(-?\d+)$n"                 => Some(_ => _.fold(false)(s => s == n.toLong))
    case r"<= *(-?\d+)$n"             => Some(_ => _.fold(false)(_ <= n.toLong))
    case r">= *(-?\d+)$n"             => Some(_ => _.fold(false)(_ >= n.toLong))
    case r"< *(-?\d+)$n"              => Some(_ => _.fold(false)(_ < n.toLong))
    case r"> *(-?\d+)$n"              => Some(_ => _.fold(false)(_ > n.toLong))
    case r"(-?\d+)$n1 *- *(-?\d+)$n2" => Some(_ => _.fold(false)(d => n1.toLong <= d && d <= n2.toLong))
    case other                        =>
      println(s"Unrecognized number filter expr: `$other`")
      None
  }

  def predDecimal(token: String): Option[Markers => Option[Double] => Boolean] = token.trim match {
    case ""                                       => None
    case "-"                                      => Some(_ => _.isEmpty)
    case "+"                                      => Some(_ => _.isDefined)
    case r"(-?\d+\.?\d*)$n"                       => Some(_ => _.fold(false)(_ == n.toDouble))
    case r"<= *(-?\d+\.?\d*)$n"                   => Some(_ => _.fold(false)(_ <= n.toDouble))
    case r">= *(-?\d+\.?\d*)$n"                   => Some(_ => _.fold(false)(_ >= n.toDouble))
    case r"< *(-?\d+\.?\d*)$n"                    => Some(_ => _.fold(false)(_ < n.toDouble))
    case r"> *(-?\d+\.?\d*)$n"                    => Some(_ => _.fold(false)(_ > n.toDouble))
    case r"(-?\d+\.?\d*)$n1 *- *(-?\d+\.?\d*)$n2" => Some(_ => _.fold(false)(d => n1.toDouble <= d && d <= n2.toDouble))
    case other                                    =>
      println(s"Unrecognized decimal filter expr: `$other`")
      None
  }

  def predBigInt(token: String): Option[Markers => Option[String] => Boolean] = token.trim match {
    case ""                           => None
    case "-"                          => Some(_ => _.isEmpty)
    case "+"                          => Some(_ => _.isDefined)
    case r"(-?\d+)$n"                 => Some(_ => _.fold(false)(s => BigInt(s) == BigInt(n)))
    case r"<= *(-?\d+)$n"             => Some(_ => _.fold(false)(s => BigInt(s) <= BigInt(n)))
    case r">= *(-?\d+)$n"             => Some(_ => _.fold(false)(s => BigInt(s) >= BigInt(n)))
    case r"< *(-?\d+)$n"              => Some(_ => _.fold(false)(s => BigInt(s) < BigInt(n)))
    case r"> *(-?\d+)$n"              => Some(_ => _.fold(false)(s => BigInt(s) > BigInt(n)))
    case r"(-?\d+)$n1 *- *(-?\d+)$n2" => Some(_ => _.fold(false)(s => BigInt(n1) <= BigInt(s) && BigInt(s) <= BigInt(n2)))
    case other                        =>
      println(s"Unrecognized BigInt filter expr: `$other`")
      None
  }

  def predBigDecimal(token: String): Option[Markers => Option[String] => Boolean] = {
    val m = (Set.empty[Long], Set.empty[Long], Set.empty[Long])
    token.trim match {
      case ""                                       => None
      case "-"                                      => Some(_ => _.isEmpty)
      case "+"                                      => Some(m => _.isDefined)
      case r"(-?\d+\.?\d*)$n"                       => Some(m => _.fold(false)(s => BigDecimal(s) == BigDecimal(n)))
      case r"<= *(-?\d+\.?\d*)$n"                   => Some(m => _.fold(false)(s => BigDecimal(s) <= BigDecimal(n)))
      case r">= *(-?\d+\.?\d*)$n"                   => Some(m => _.fold(false)(s => BigDecimal(s) >= BigDecimal(n)))
      case r"< *(-?\d+\.?\d*)$n"                    => Some(m => _.fold(false)(s => BigDecimal(s) < BigDecimal(n)))
      case r"> *(-?\d+\.?\d*)$n"                    => Some(m => _.fold(false)(s => BigDecimal(s) > BigDecimal(n)))
      case r"(-?\d+\.?\d*)$n1 *- *(-?\d+\.?\d*)$n2" => Some(m => _.fold(false)(s => BigDecimal(n1) <= BigDecimal(s) && BigDecimal(s) <= BigDecimal(n2)))
      case other                                    =>
        println(s"Unrecognized BigDecimal filter expr: `$other`")
        None
    }
  }
}
