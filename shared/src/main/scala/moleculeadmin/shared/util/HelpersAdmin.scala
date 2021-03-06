package moleculeadmin.shared.util

import java.net.URI
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZonedDateTime}
import java.util.{Date, UUID}
import molecule.ast.query.{CollectionBinding, InDataSource, InVar, Query, RelationBinding}
import molecule.util.Helpers

trait HelpersAdmin extends Helpers with SpecialNames {

  def firstLow(str: Any): String = s"${str.toString.head.toLower}${str.toString.tail}"

  def mandatory(attr: String): Boolean = attr.last != '_' && attr.last != '$'

  implicit class capitalized2lower(cap: String) {
    def low: String = if (cap.nonEmpty) firstLow(cap) else ""
  }

  def ms2ldt(ms: Long): LocalDateTime = {
    LocalDateTime.ofInstant(Instant.ofEpochMilli(ms), localZoneOffset)
  }

  def str2ldt(str: String): LocalDateTime = {
    val ms0 = str2date(str).getTime
    val ms  = ms0 - daylight(ms0)
    LocalDateTime.ofInstant(Instant.ofEpochMilli(ms), localZoneOffset)
  }

  def ldt2str(ldt: LocalDateTime): String = {
    if ((ldt.getNano / 1000000) > 0) {
      ldt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS"))
    } else if (ldt.getSecond != 0) {
      ldt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    } else if (ldt.getHour != 0 || ldt.getMinute != 0) {
      ldt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
    } else {
      ldt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    }
  }

  def _html2str(html: String): String = {
    val str = html
      .replaceFirst("<span[^>]*>", "")
      .replaceFirst("</span>", "")
      .replace("<span></span>", "")
      .replace("&nbsp;", " ")
      .replace("&amp;", "&")
      .replace("&lt;", "<")
      .replace("&gt;", ">")
      .replace("<br>", "\n")
    if (str.startsWith("{"))
    // Invisible empty strings surrounded with { }
      if (str.endsWith("}") && str.tail.init.replace("\n", " ").trim.isEmpty)
        str.tail.init
      else
        str
    else
      str
  }

  def renderValue(v: Any): String = v match {
    case d: Date                            => date2str(d.asInstanceOf[Date])
    case s: String if s.startsWith("__n__") => s.drop(5)
    case _                                  => v.toString
  }

  implicit class StrIsX(str: String) {
    def isInt = scala.util.Try(str.toInt).isSuccess
    def isLong = scala.util.Try(str.toLong).isSuccess
    def isDouble = scala.util.Try(str.toDouble).isSuccess
    def isFloat = scala.util.Try(str.toFloat).isSuccess
    def isBigInt = scala.util.Try(BigInt(str)).isSuccess
    def isBigDecimal = scala.util.Try(BigDecimal(str)).isSuccess
    def isBoolean = scala.util.Try(str.toBoolean).isSuccess
  }

  case class Timer(txt: String = "time", on: Boolean = true) {
    val time0 = System.currentTimeMillis
    var time1 = time0
    //    println("timer start ---------------------------------")

    def delta: Long = {
      val time2 = System.currentTimeMillis - time1
      time1 = System.currentTimeMillis()
      time2
    }

    def ms: String = thousands(delta) + " ms"
    def msTotal: String = thousands(System.currentTimeMillis() - time0) + " ms"

    def log(n: Int): Unit = {
      val time2 = System.currentTimeMillis - time1
      if (on) println(s"$txt $n: " + "%10d".format(time2))
      time1 = System.currentTimeMillis()
    }
    def total: Unit = if (on) println(
      s"$txt saving time: ${thousands(System.currentTimeMillis - time0)} ms"
    )
  }

  def prettyMillisDelta(millisDelta: Long): String = {
    val second = 1000L
    val minute = second * 60
    val hour   = minute * 60
    val day    = hour * 24
    val month  = day * 30
    val year   = day * 365

    if (millisDelta / year > 1) s"${millisDelta / year} years ago"
    else if (millisDelta / year == 1) "1 year"
    else if (millisDelta / month > 1) s"${millisDelta / month} months ago"
    else "xxx"
  }

  def thousands(i: Long): String =
    i.toString.reverse.grouped(3).mkString(" ").reverse


  // Todo - hack to transmit input types because we can't transfer them typed with boopickle - maybe use upickle instead here?
  def c(arg: Any): String = arg match {
    case _: String     => "String"
    case _: Int        => "Long"
    case _: Long       => "Long"
    case _: Float      => "Double"
    case _: Double     => "Double"
    case _: BigInt     => "BigInt"
    case _: BigDecimal => "BigDecimal"
    case _: Boolean    => "Boolean"
    case _: Date       => "Date"
    case _: UUID       => "UUID"
    case _: URI        => "URI"
  }

  def encodeInputs(q: Query): (
    Seq[(Int, (String, String))],
      Seq[(Int, Seq[(String, String)])],
      Seq[(Int, Seq[Seq[(String, String)]])]
    ) = q.i.inputs.zipWithIndex.foldLeft(
    Seq.empty[(Int, (String, String))],
    Seq.empty[(Int, Seq[(String, String)])],
    Seq.empty[(Int, Seq[Seq[(String, String)]])]
  ) {
    case ((l, ll, lll), (InVar(RelationBinding(_), argss), i))   => (l, ll, lll :+ (i, argss.map(v => v.map(w => (c(w), w.toString)))))
    case ((l, ll, lll), (InVar(CollectionBinding(_), argss), i)) => (l, ll :+ (i, argss.head.map(v => (c(v), v.toString))), lll)
    case ((l, ll, lll), (InVar(_, argss), i))                    => (l :+ (i, (c(argss.head.head), argss.head.head.toString)), ll, lll)
    case ((l, ll, lll), (InDataSource(_, argss), i))             => (l :+ (i, (c(argss.head.head), argss.head.head.toString)), ll, lll)
    case other                                                   => sys.error(s"[molecule.ops.QueryOps] UNEXPECTED inputs: $other")
  }

}
