package moleculeadmin.server.utils
import java.lang.{Boolean => jBoolean, Double => jDouble, Long => jLong}
import java.math.{BigDecimal => jBigDec, BigInteger => jBigInt}
import java.net.URI
import java.util.{Date, UUID, Map => jMap}
import clojure.lang.{Keyword, LazySeq, PersistentHashSet, PersistentVector}
import moleculeadmin.shared.lib.moleculeExtras.HelpersAdmin
import scala.collection.JavaConverters._

object rows2json extends HelpersAdmin {

  def apply(rows: java.util.Iterator[java.util.List[AnyRef]],
            attrs0: Seq[(Int, String, String, Int, Boolean, String)],
            index: Boolean = true,
            maxRows: Int = 999999999
           ): String = {

    val attrs = attrs0.map { case (j, field, tpe, card, optional, aggr) =>
      tpe match {
        case r"Map\[String,(.*)$t\]" if optional => (6, j, field, t, card, optional)
        case r"Set\[(.*)$t\]" if optional        => (5, j, field, t, card, optional)
        case t if optional                       => (4, j, field, t, card, optional)
        case t if card >= 3                      => (3, j, field, t, card, optional)
        case t if card == 2 && aggr.nonEmpty     => (1, j, field, t, card, optional)
        case t if card == 2                      => (2, j, field, t, card, optional)
        case t if aggr == "set"                  => (2, j, field, t, card, optional)
        case t if aggr == "vector"               => (7, j, field, t, card, optional)
        case t if aggr == "lazySeq"              => (8, j, field, t, card, optional)
        case t                                   => (1, j, field, t, card, optional)
      }
    }

    //        println("==================================================")

    // Json array start
    val buf = new StringBuilder("[")
    var i = 1
    var row: java.util.List[AnyRef] = null
    while (rows.hasNext && i <= maxRows) {
      row = rows.next()

      // Separate to next row object
      if (i > 1) buf.append(",")

      //      if(i <= 10) println("row ---------: " + row)

      // Row object start
      buf.append("{")
      attrs.foreach { case (f, j, field, tpe, card, optional) =>
        if (j == 0 && index)
          buf.append("'i':").append(i).append(",")

        //                if(i <= 10) println(j + "   " + field)

        val value = row.get(j)

        if (j > 0)
          buf.append(", ")

        buf.append(s"'$field':")

        if (value == null) {
          buf.append("null")
        } else {
          f match {
            case 1 => outValue(buf, value, tpe)
            case 2 => outSet(buf, value, tpe)
            case 3 => outMap(buf, value, tpe)
            case 4 => outOptionValue(buf, value, tpe)
            case 5 => outOptionSet(buf, value, tpe)
            case 6 => outOptionMap(buf, value, tpe)
            case 7 => outVector(buf, value, tpe)
            case 8 => outLazySeq(buf, value, tpe)
          }
        }
      }
      // Row object end
      buf.append("}")
      i += 1
    }

    // Json array end
    buf.append("]")
    buf.result()
  }


  def outValue(buf: StringBuilder, value: Any, tpe: String) = tpe match {
    case "String"  => quote(buf, value.toString)
    case "Int"     => buf.append(value)
    case "Float"   => buf.append(value)
    case "Date"    => quote(buf, sdfDate.format(value.asInstanceOf[Date]))
    case "UUID"    => quote(buf, value.toString)
    case "URI"     => quote(buf, value.toString)
    case "Boolean" => buf.append(value.toString)
    case _         => buf.append(value.toString)
  }

  def outSet(buf: StringBuilder, value: Any, tpe: String) = {
    val values = value.asInstanceOf[PersistentHashSet].asScala.toSeq
    tpe match {
      case "String"  => renderArray(buf, values, true)
      case "Date"    => renderArray(buf, values.map(v => sdfDate.format(v.asInstanceOf[Date])), true)
      case "UUID"    => renderArray(buf, values, true)
      case "URI"     => renderArray(buf, values, true)
      case "Boolean" => renderArray(buf, values, false)
      case _         => renderArray(buf, values, false)
    }
  }

  def outVector(buf: StringBuilder, value: Any, tpe: String) = {
    val values = value.asInstanceOf[PersistentVector].asScala.toSeq
    tpe match {
      case "String"  => renderArray(buf, values, true)
      case "Date"    => renderArray(buf, values.map(v => sdfDate.format(v.asInstanceOf[Date])), true)
      case "UUID"    => renderArray(buf, values, true)
      case "URI"     => renderArray(buf, values, true)
      case "Boolean" => renderArray(buf, values, false)
      case _         => renderArray(buf, values, false)
    }
  }

  def outLazySeq(buf: StringBuilder, value: Any, tpe: String) = {
    val values = value.asInstanceOf[LazySeq].asScala.toSeq
    tpe match {
      case "String"  => renderArray(buf, values, true)
      case "Date"    => renderArray(buf, values.map(v => sdfDate.format(v.asInstanceOf[Date])), true)
      case "UUID"    => renderArray(buf, values, true)
      case "URI"     => renderArray(buf, values, true)
      case "Boolean" => renderArray(buf, values, false)
      case _         => renderArray(buf, values, false)
    }
  }


  def outMap(buf: StringBuilder, value: Any, tpe: String) = {
    val values = value.asInstanceOf[PersistentHashSet].asScala.toSeq
    tpe match {
      case "String"  => renderObj(buf, values, true)
      case "Date"    =>
        var firstInObj = true
        buf.append("{")
        values.foreach { case s: String =>
          val p = s.split("@", 2)
          val (k, date) = (p(0), sdfDate.parse(p(1)))

          if (firstInObj) {
            firstInObj = false
          } else {
            buf.append(", ")
          }

          buf.append(k).append(": ")
          quote(buf, sdfDate.format(date))
        }
        buf.append("}")
      case "UUID"    => renderObj(buf, values, true)
      case "URI"     => renderObj(buf, values, true)
      case "Boolean" => renderObj(buf, values, false)
      case _         => renderObj(buf, values, false)
    }
  }


  def outOptionValue(buf: StringBuilder, value: Any, tpe: String) = tpe match {
    case "String" => value match {
      case v: String                             => quote(buf, v)
      case v if v.toString.contains(":db/ident") => val s = v.toString; quote(buf, s.substring(s.lastIndexOf("/") + 1).init.init)
      case v                                     => quote(buf, v.asInstanceOf[jMap[String, String]].asScala.toMap.values.head) // pull result map: {:Ns/str "abc"}
    }

    case "Int" => value match {
      case v => buf.append(v.asInstanceOf[jMap[String, jLong]].asScala.toMap.values.head.toString) // pull result map: {:Ns/int 42}
    }

    case "Float" => value match {
      case v: jDouble => buf.append(v)
      case v          => buf.append(v.asInstanceOf[jMap[String, jDouble]].asScala.toMap.values.head.toString)
    }

    case "Long" => value match {
      case v: jLong                           => buf.append(v)
      case v if v.toString.contains(":db/id") => val s = v.toString; buf.append(s.substring(s.lastIndexOf("/") + 1).init.init)
      case v                                  => buf.append(v.asInstanceOf[jMap[String, jLong]].asScala.toMap.values.head.toString)
    }

    case "Double" => value match {
      case v: jDouble => buf.append(v)
      case v          => buf.append(v.asInstanceOf[jMap[String, jDouble]].asScala.toMap.values.head.toString)
    }

    case "Boolean" => value match {
      case v: jBoolean => buf.append(v)
      case v           => buf.append(v.asInstanceOf[jMap[String, jBoolean]].asScala.toMap.values.head.toString)
    }

    case "BigInt" => value match {
      case v: jBigInt => buf.append(v)
      case v          => buf.append(v.asInstanceOf[jMap[String, jBigInt]].asScala.toMap.values.head.toString)
    }

    case "BigDecimal" => value match {
      case v: jBigDec => buf.append(v)
      case v          => buf.append(v.asInstanceOf[jMap[String, jBigDec]].asScala.toMap.values.head.toString)
    }

    case "Date" => value match {
      case v: Date => quote(buf, sdfDate.format(v.asInstanceOf[Date]))
      case v       => quote(buf, sdfDate.format(v.asInstanceOf[jMap[String, Date]].asScala.toMap.values.head))
    }

    case "UUID" => value match {
      case v: UUID => quote(buf, v.toString)
      case v       => quote(buf, v.asInstanceOf[jMap[String, UUID]].asScala.toMap.values.head.toString)
    }

    case "URI" => value match {
      case v: URI => quote(buf, v.toString)
      case v      => quote(buf, v.asInstanceOf[jMap[String, URI]].asScala.toMap.values.head.toString)
    }
  }


  def outOptionSet(buf: StringBuilder, value: Any, tpe: String) = tpe match {
    case "String" => value match {
      case vs: PersistentHashSet => renderArray(buf, vs.asScala, true)

      // {:Ns/enums [{:db/ident :Ns.enums/enum1} {:db/ident :Ns.enums/enum2}]}
      case vs if vs.toString.contains(":db/ident") =>
        val identMaps = vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala
        val enums = identMaps.map(_.asInstanceOf[jMap[String, Keyword]].asScala.toMap.values.head.getName)
        renderArray(buf, enums, true)

      // {:Ns/strs ["a" "b" "c"]}
      case vs => renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, true)
    }

    case "Int" => value match {
      case vs: PersistentHashSet => renderArray(buf, vs.asScala, false)
      case vs                    => renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Float" => value match {
      case vs: PersistentHashSet => renderArray(buf, vs.asScala, false)
      case vs                    => renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Long" => value match {
      case vs: PersistentHashSet =>
        renderArray(buf, vs.asScala, false)

      // {:Ns/ref1 [{:db/id 3} {:db/id 4}]}
      case vs if vs.toString.contains(":db/id") =>
        val idMaps = vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala
        renderArray(buf, idMaps.map(_.asInstanceOf[jMap[String, Long]].asScala.toMap.values.head), false)

      // {:Ns/longs [3 4 5]}
      case vs =>
        renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Double" => value match {
      case vs: PersistentHashSet => renderArray(buf, vs.asScala, false)
      case vs                    => renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Boolean" => value match {
      case vs: PersistentHashSet => renderArray(buf, vs.asScala, false)
      case vs                    => renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "BigInt" => value match {
      case vs: PersistentHashSet => renderArray(buf, vs.asScala, false)
      case vs                    => renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }


    case "BigDecimal" => value match {
      case vs: PersistentHashSet => renderArray(buf, vs.asScala, false)
      case vs                    => renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Date" => value match {
      case vs: PersistentHashSet =>
        val values = vs.asScala.map(_.asInstanceOf[Date])
        renderArray(buf, values.map(v => sdfDate.format(v)), true)
      case vs                    =>
        val values = vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala
        renderArray(buf, values.map(v => sdfDate.format(v.asInstanceOf[Date])), true)
    }

    case "UUID" => value match {
      case vs: PersistentHashSet => renderArray(buf, vs.asScala.map(_.toString), true)
      case vs                    => renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, true)
    }

    case "URI" => value match {
      case vs: PersistentHashSet => renderArray(buf, vs.asScala.map(_.toString), true)
      case vs                    => renderArray(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, true)
    }
  }


  def outOptionMap(buf: StringBuilder, value: Any, tpe: String) = tpe match {
    case "String" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala, true)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, true)
    }

    case "Int" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala, false)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Long" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala, false)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Float" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala, false)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Double" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala, false)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Boolean" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala, false)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "BigInt" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala, false)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "BigDecimal" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala, false)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, false)
    }

    case "Date" => value match {
      case vs: PersistentHashSet =>
        val values = vs.asScala
        var firstInObj = true
        buf.append("{")
        values.foreach { case s: String =>
          val p = s.split("@", 2)
          val (k, d) = (p(0), sdfDate.parse(p(1)))

          if (firstInObj) {
            firstInObj = false
          } else {
            buf.append(", ")
          }

          buf.append(k).append(": ")
          quote(buf, sdfDate.format(d))
        }
        buf.append("}")

      case vs =>
        val values = vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala
        var firstInObj = true
        buf.append("{")
        values.foreach { case s: String =>
          val p = s.split("@", 2)
          val (k, d) = (p(0), sdfDate.parse(p(1)))

          if (firstInObj) {
            firstInObj = false
          } else {
            buf.append(", ")
          }

          buf.append(k).append(": ")
          quote(buf, sdfDate.format(d))
        }
        buf.append("}")
    }

    case "UUID" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala.map(_.toString), true)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, true)
    }

    case "URI" => value match {
      case vs: PersistentHashSet => renderObj(buf, vs.asScala.map(_.toString), true)
      case vs                    => renderObj(buf, vs.asInstanceOf[jMap[String, PersistentVector]].asScala.toMap.values.head.asScala, true)
    }
  }


  def renderArray(buf: StringBuilder, values: Iterable[Any], quoting: Boolean) = {
    var firstInArray = true
    buf.append("[")
    val quotedValues = values.foreach { value =>
      if (firstInArray) {
        firstInArray = false
      } else {
        buf.append(", ")
      }
      if (quoting)
        quote(buf, value.toString)
      else
        buf.append(value.toString)
    }
    buf.append("]")
  }

  def renderObj(buf: StringBuilder, values: Iterable[Any], quoting: Boolean) = {
    var firstInObj = true
    buf.append("{")
    values.foreach { case s: String =>
      val p = s.split("@", 2)
      val (k, v) = (p(0), p(1))

      if (firstInObj) {
        firstInObj = false
      } else {
        buf.append(", ")
      }

      buf.append(k).append(": ")
      if (quoting)
        quote(buf, v)
      else
        buf.append(v)
    }
    buf.append("}")
  }


  def quote(buf: StringBuilder, value: Any): Unit = {
    buf.append('"') //open quote
    appendEscapedString(buf, value.toString)
    buf.append('"') //close quote
  }

  def appendEscapedString(buf: StringBuilder, s: String): Unit = {
    s.foreach { c =>
      val strReplacement = c match {
        case '"'  => "\\\""
        case '\\' => "\\\\"
        case '\b' => "\\b"
        case '\f' => "\\f"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        // Set.contains will cause boxing of c to Character, try and avoid this
        case c if (c >= '\u0000' && c < '\u0020') || jsEscapeChars.contains(c) =>
          "\\u%04x".format(c: Int)

        case _ => ""
      }

      // Use Char version of append if we can, as it's cheaper.
      if (strReplacement.isEmpty) {
        buf.append(c)
      } else {
        buf.append(strReplacement)
      }
    }
  }

  val jsEscapeChars: Set[Char] =
    List(('\u00ad', '\u00ad'),
      ('\u0600', '\u0604'),
      ('\u070f', '\u070f'),
      ('\u17b4', '\u17b5'),
      ('\u200c', '\u200f'),
      ('\u2028', '\u202f'),
      ('\u2060', '\u206f'),
      ('\ufeff', '\ufeff'),
      ('\ufff0', '\uffff'))
      .foldLeft(Set[Char]()) {
        case (set, (start, end)) =>
          set ++ (start to end).toSet
      }
}
