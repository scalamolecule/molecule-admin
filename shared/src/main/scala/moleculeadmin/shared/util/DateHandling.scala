package moleculeadmin.shared.util

import java.time._
import java.time.format.DateTimeFormatter
import java.util.Date


trait DateHandling extends RegexMatching {

  def date2strFull(date: Date, zone: ZoneOffset = ZoneOffset.UTC): String = {
    // todo: use as example of catching non-js compliant code with `sbt sharedJS/test`
    //    val ldt = LocalDateTime.ofInstant(date.toInstant, zone) // Todo
    val ldt = LocalDateTime.ofInstant(Instant.ofEpochMilli(date.getTime), zone)
    val zdt = ZonedDateTime.of(ldt, zone)
    zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX"))
  }


  def dateLocal2str(date: Date): String =
    dateZone2str(date, OffsetDateTime.now().getOffset)

  def dateZone2str(date: Date, zone: ZoneOffset = ZoneOffset.UTC): String = {
    val ldt = LocalDateTime.ofInstant(Instant.ofEpochMilli(date.getTime), zone)
    val zdt = ZonedDateTime.of(ldt, zone)
    val ms  = (ldt.getNano / 1000000) > 0
    //    println("ms / nano: " + zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")))
    //    println("ms / nano: " + (ldt.getNano / 1000000) + "   -   " + ldt.getNano)

    if (ms && zone != ZoneOffset.UTC) {
      zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS XXX"))
    } else if (zone != ZoneOffset.UTC) {
      zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss XXX"))
    } else if (ms) {
      zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS"))
    } else if (ldt.getSecond != 0) {
      zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    } else if (ldt.getHour != 0 || ldt.getMinute != 0) {
      zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
    } else {
      zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    }
  }

  def error(err: String): Nothing = {
    val err1 = "[DateHandling]  " + err
    println(err1)
    throw new IllegalArgumentException(err1)
  }

  // OBS: seems it can't be used from client side
  def str2dateLocal(s: String): Date =
    str2date(s, OffsetDateTime.now().getOffset, false)

  def str2date(s: String,
               zoneOffset: ZoneOffset = ZoneOffset.UTC,
               zoned: Boolean = true
              ): Date = {

    def mkMs(s: String): Int = (s match {
      case r"(00\d|0\d\d|\d\d\d)$n" => n.toInt
      case r"(0\d|\d\d)$n"          => n.toInt * 10
      case r"(\d)$n"                => n.toInt * 100
    }) * 1000000

    def mkZh(zh0: String): Int = if (zh0 == "Z") 0 else zh0.toInt
    def mkZm(zh0: String, zm0: String): Int = if (zh0.head == 'Z')
      0 else zm0.toInt

    def da(y0: String,
           m0: String = "1",
           d0: String = "1",
           hh0: String = "0",
           mm0: String = "0",
           ss0: String = "0",
           ms0: String = "0",
           zh0: String = "0",
           zm0: String = "0"
          ): Long = {
      val (y, m, d, hh, mm, ss, ms, zh, zm) = (
        y0.toInt, m0.toInt, d0.toInt,
        hh0.toInt, mm0.toInt, ss0.toInt, mkMs(ms0),
        mkZh(zh0), mkZm(zh0, zm0)
      )

      println("")
      println((y, m, d, hh, mm, ss, ms, zh, zm))
      println(LocalDateTime.of(y, m, d, hh, mm, ss, ms))
      println(LocalDateTime.of(y, m, d, hh, mm, ss, ms).toInstant(zoneOffset).getEpochSecond)
      println(LocalDateTime.of(y, m, d, hh, mm, ss, ms).atZone(zoneOffset).toInstant.getEpochSecond)

      val inst = if (zm != 0) {
        LocalDateTime.of(y, m, d, hh, mm, ss, ms)
          .atZone(ZoneOffset.ofHoursMinutes(zh, zm)).toInstant
      } else if (zh != 0) {
        LocalDateTime.of(y, m, d, hh, mm, ss, ms)
          .atZone(ZoneOffset.ofHours(zh)).toInstant
      } else {
        LocalDateTime.of(y, m, d, hh, mm, ss, ms)
          .atZone(zoneOffset).toInstant
      }
      inst.getEpochSecond * 1000 + inst.getNano / 1000000
    }


    lazy val milliSecondsX: Long = s.trim match {
      case r"^(\d{1,4})$y$$"                                                                                                                                                                                               => da(y)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m$$"                                                                                                                                                                            => da(y, m)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d$$"                                                                                                                                                => da(y, m, d)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm$$"                                                                                         => da(y, m, d, hh, mm)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss$$"                                                                 => da(y, m, d, hh, mm, ss)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms$$"                                                   => da(y, m, d, hh, mm, ss, ms)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms *([\+\-]?)$sign(\d{1,2}|Z)$zh$$" if zoned            => da(y, m, d, hh, mm, ss, ms, sign + zh)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms *([\+\-]?)$sign(\d{1,2})$zh:(\d{1,2})$zm$$" if zoned => da(y, m, d, hh, mm, ss, ms, sign + zh, sign + zm)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss *([\+\-]?)$sign(\d{1,2}|Z)$zh$$" if zoned                          => da(y, m, d, hh, mm, ss, "0", sign + zh)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss *([\+\-]?)$sign(\d{1,2})$zh:(\d{1,2})$zm$$" if zoned               => da(y, m, d, hh, mm, ss, "0", sign + zh, sign + zm)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm *([\+\-]?)$sign(\d{1,2}|Z)$zh$$" if zoned                                                  => da(y, m, d, hh, mm, "0", "0", sign + zh)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm *([\+\-]?)$sign(\d{1,2})$zh:(\d{1,2})$zm$$" if zoned                                       => da(y, m, d, hh, mm, "0", "0", sign + zh, sign + zm)
      case other                                                                                                                                                                                                           =>
        error(s"Unrecognized date pattern: `$other`")
    }

    val milliSeconds: Long = s.trim match {
      case r"^(\d{1,4})$y$$"                                                                                                                                                                                      => da(y)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m$$"                                                                                                                                                                   => da(y, m)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d$$"                                                                                                                                       => da(y, m, d)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm$$"                                                                                => da(y, m, d, hh, mm)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss$$"                                                        => da(y, m, d, hh, mm, ss)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms$$"                                          => da(y, m, d, hh, mm, ss, ms)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms *([\+\-]?)$sign(\d{1,2}|Z)$zh$$"            => da(y, m, d, hh, mm, ss, ms, sign + zh)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms *([\+\-]?)$sign(\d{1,2})$zh:(\d{1,2})$zm$$" => da(y, m, d, hh, mm, ss, ms, sign + zh, sign + zm)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss *([\+\-]?)$sign(\d{1,2}|Z)$zh$$"                          => da(y, m, d, hh, mm, ss, "0", sign + zh)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss *([\+\-]?)$sign(\d{1,2})$zh:(\d{1,2})$zm$$"               => da(y, m, d, hh, mm, ss, "0", sign + zh, sign + zm)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm *([\+\-]?)$sign(\d{1,2}|Z)$zh$$"                                                  => da(y, m, d, hh, mm, "0", "0", sign + zh)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm *([\+\-]?)$sign(\d{1,2})$zh:(\d{1,2})$zm$$"                                       => da(y, m, d, hh, mm, "0", "0", sign + zh, sign + zm)
      case other                                                                                                                                                                                                  =>
        error(s"Unrecognized date pattern: `$other`")
    }

    println(milliSeconds)

    new Date(milliSeconds)
  }

  def truncateDateStr(dateStr: String): String = truncateDateStrFull(dateStr, false)

  def truncateDateStrFull(dateStr: String, zoned: Boolean = true): String = if (zoned) {
    dateStr match {
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}"                                                           => s"$ymd"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*00:00:00\.000[+ -]*00:00"                             => s"$ymd"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*00:00:00\.000[+ -]"                                   => s"$ymd"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*00:00:00[+ -]*00:00"                                  => s"$ymd"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]"                                                      => s"$ymd"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d)$hm:00\.000[+ -]*00:00"                    => s"$ymd $hm"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d)$hm:00\.000"                               => s"$ymd $hm"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d)$hm:00[+ -]*00:00"                         => s"$ymd $hm"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d)$hm:00([+ -]*\d\d:\d\d)$z"                 => s"$ymd $hm$z"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d)$hm:00"                                    => s"$ymd $hm"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d)$hm"                                       => s"$ymd $hm"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms\.000[+ -]*00:00"                 => s"$ymd $hms"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms\.000([+ -]*\d\d:\d\d)$z"         => s"$ymd $hms$z"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms[+ -]*00:00"                      => s"$ymd $hms"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms([+ -]*\d\d:\d\d)$z"              => s"$ymd $hms$z"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms\.000"                            => s"$ymd $hms"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms"                                 => s"$ymd $hms"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms\.(\d\d\d)$ms[+ -]*00:00"         => s"$ymd $hms.$ms"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms\.(\d\d\d)$ms([+ -]*\d\d:\d\d)$z" => s"$ymd $hms.$ms$z"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms\.(\d\d\d)$ms"                    => s"$ymd $hms.$ms"
      case other                                                                                   =>
        error(s"Can't truncate unrecognized zoned date pattern: `$other`")
    }
  } else {
    dateStr match {
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}"                                          => s"$ymd"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*00:00:00\.000.*"                     => s"$ymd"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d)$hm:00\.000.*"            => s"$ymd $hm"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d)$hm:00[+ -]*\d\d:\d\d"    => s"$ymd $hm"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms\.000.*"         => s"$ymd $hms"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms[+ -]*\d\d:\d\d" => s"$ymd $hms"
      case r"(\d\d\d\d-\d\d-\d\d)${ymd}['T ]*(\d\d:\d\d:\d\d)$hms\.(\d\d\d)$ms.*" => s"$ymd $hms.$ms"
      case other                                                                  =>
        error(s"Can't truncate unrecognized date pattern: `$other`")
    }
  }

  def expandDateStr(dateStr: String) = {
    def p(s: String, i: Int = 2): String = i match {
      case 2 => "%02d".format(s.toInt)
      case 3 => "%03d".format(s.toInt)
      case 4 => "%04d".format(s.toInt)
    }

    dateStr match {
      case r"^(\d{1,4})$y$$"                                                                                                                                             => s"${p(y, 4)}-01-01 00:00:00.000"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m$$"                                                                                                                          => s"${p(y, 4)}-${p(m)}-01 00:00:00.000"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d$$"                                                                                              => s"${p(y, 4)}-${p(m)}-${p(d)} 00:00:00.000"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh$$"                                                               => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:00:00.000"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm$$"                                       => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:00.000"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss$$"               => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}.000"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d[ T]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms$$" => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}.${p(ms, 3)}"
      case other                                                                                                                                                         =>
        error(s"Can't expand unrecognized date pattern: `$other`")
    }
  }
}
