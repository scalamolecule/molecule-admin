package moleculeadmin.shared.util

import java.time._
import java.time.format.DateTimeFormatter
import java.util.Date


trait DateHandling extends RegexMatching {

  lazy val localZoneOffset: ZoneOffset = OffsetDateTime.now().getOffset
  lazy val localOffset    : String     = localZoneOffset.toString

  private def testing(): Boolean = {

    // http://cquiroz.github.io/scala-java-time/

    // always returns 2009-02-13T23:31:30
    val fixedClock = Clock.fixed(Instant.ofEpochSecond(1234567890L), ZoneOffset.ofHours(0))
    // fixedClock: java.time.Clock = FixedClock[2009-02-13T23:31:30Z,Z]

    val date = LocalDateTime.now(fixedClock)
    // date: java.time.LocalDateTime = 2009-02-13T23:31:30

    date.getMonth == Month.FEBRUARY
    // res1: Boolean = true

    date.getDayOfWeek == DayOfWeek.FRIDAY
    // res2: Boolean = true

    date.getDayOfMonth == 13
    // res3: Boolean = true

    date.getHour == 23
    // res4: Boolean = true

    val tomorrow = date.plusDays(1)
    // tomorrow: java.time.LocalDateTime = 2009-02-14T23:31:30

    val duration = Duration.between(date, tomorrow)
    // duration: java.time.Duration = PT24H

    duration.toMinutes == 1440L
    // res5: Boolean = true

    val period = Period.between(date.toLocalDate, tomorrow.toLocalDate)
    // period: java.time.Period = P1D

    period.get(temporal.ChronoUnit.DAYS) == 1L
    // res6: Boolean = true

    val date1 = LocalDate.of(2001, 1, 31)
    // date1: java.time.LocalDate = 2001-01-31

    date1.plusMonths(1) == LocalDate.of(2001, 2, 28)
    // res7: Boolean = true

    val date2 = date1.`with`(temporal.TemporalAdjusters.next(DayOfWeek.SUNDAY))
    // date2: java.time.LocalDate = 2001-02-04

    date2 == LocalDate.of(2001, 2, 4)
    // res8: Boolean = true

    val offsetTime = OffsetTime.of(date.toLocalTime, ZoneOffset.ofHours(1))
    // offsetTime: java.time.OffsetTime = 23:31:30+01:00

    offsetTime.isBefore(OffsetTime.of(date.toLocalTime, ZoneOffset.ofHours(0)))
    // res9: Boolean = true

    val format1 = format.DateTimeFormatter.ofPattern("MMMM MM d HH mm ss EE EEEE yyyy G", java.util.Locale.GERMAN)
    // format1: java.time.format.DateTimeFormatter = Text(MonthOfYear)' 'Value(MonthOfYear,2)' 'Value(DayOfMonth)' 'Value(HourOfDay,2)' 'Value(MinuteOfHour,2)' 'Value(SecondOfMinute,2)' 'Text(DayOfWeek,SHORT)' 'Text(DayOfWeek)' 'Value(YearOfEra,4,19,EXCEEDS_PAD)' 'Text(Era,SHORT)

    date.format(format1) == "Februar 02 13 23 31 30 Fr. Freitag 2009 n. Chr."
    // res10: Boolean = false

    chrono.JapaneseDate.now(fixedClock).toString == "Japanese Heisei 21-02-13"
    // res11: Boolean = true

    chrono.ThaiBuddhistDate.now(fixedClock).toString == "ThaiBuddhist BE 2552-02-13"
    // res12: Boolean = true

    chrono.MinguoDate.now(fixedClock).toString == "Minguo ROC 98-02-13"
    // res13: Boolean = true
  }

  private def error(err: String): Nothing = {
    val err1 = "[DateHandling]  " + err
    println(err1)
    throw new IllegalArgumentException(err1)
  }

  private def mkMs(s: String): Int = s match {
    case r"(00\d|0\d\d|\d\d\d)$n" => n.toInt
    case r"(0\d|\d\d)$n"          => n.toInt * 10
    case r"(\d)$n"                => n.toInt * 100
  }

  private def local(sign: String, zh: String, zm: String): Boolean = {
    s"$sign$zh:$zm" == localOffset || zh == "Z"
  }

  private def p(s: String, i: Int = 2): String = i match {
    case 2 => "%02d".format(s.toInt)
    case 3 => "%03d".format(s.toInt)
    case 4 => "%04d".format(s.toInt)
  }


  def date2str(date: Date,
               zoneOffset: ZoneOffset = localZoneOffset): String = {
    val ldt = LocalDateTime.ofInstant(Instant.ofEpochMilli(date.getTime), zoneOffset)
    val zdt = ZonedDateTime.of(ldt, zoneOffset)
    val ms  = (ldt.getNano / 1000000) > 0
    if (zoneOffset == localZoneOffset) {
      if (ms) {
        zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS"))
      } else if (ldt.getSecond != 0) {
        zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      } else if (ldt.getHour != 0 || ldt.getMinute != 0) {
        zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
      } else {
        zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
      }
    } else {
      if (ms) {
        zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS XXX"))
      } else if (ldt.getSecond != 0) {
        zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss XXX"))
      } else {
        zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm XXX"))
      }
    }
  }

  def str2date(s: String,
               zoneOffset: ZoneOffset = localZoneOffset,
              ): Date = {

    def mkZh(zh0: String): Int =
      if (zh0.contains("Z")) 0 else zh0.toInt
    def mkZm(zh0: String, zm0: String): Int =
      if (zh0.contains("Z")) 0 else zm0.toInt

    def da(y0: String,
           m0: String = "1",
           d0: String = "1",
           hh0: String = "0",
           mm0: String = "0",
           ss0: String = "0",
           ms0: String = "0",
           zh0: String = "Z",
           zm0: String = "0"
          ): Long = {
      val (y, m, d, hh, mm, ss, nanoS, zh, zm) = (
        y0.toInt, m0.toInt, d0.toInt,
        hh0.toInt, mm0.toInt, ss0.toInt, mkMs(ms0) * 1000000,
        mkZh(zh0), mkZm(zh0, zm0)
      )

      val inst =
        if (zh0.contains("Z")) {
          LocalDateTime.of(y, m, d, hh, mm, ss, nanoS)
            .atZone(localZoneOffset).toInstant
        } else if (zm != 0) {
          LocalDateTime.of(y, m, d, hh, mm, ss, nanoS)
            .atZone(ZoneOffset.ofHoursMinutes(zh, zm)).toInstant
        } else if (zh != 0) {
          LocalDateTime.of(y, m, d, hh, mm, ss, nanoS)
            .atZone(ZoneOffset.ofHours(zh)).toInstant
        } else {
          LocalDateTime.of(y, m, d, hh, mm, ss, nanoS)
            .atZone(zoneOffset).toInstant
        }
      inst.getEpochSecond * 1000 + inst.getNano / 1000000
    }

    val milliSeconds: Long = s.trim match {
      case r"^(\d{1,4})$y$$"                                                                                                                                                                                                         => da(y)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m$$"                                                                                                                                                                                      => da(y, m)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d$$"                                                                                                                                                          => da(y, m, d)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm$$"                                                                                                  => da(y, m, d, hh, mm)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss$$"                                                                          => da(y, m, d, hh, mm, ss)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms$$"                                                            => da(y, m, d, hh, mm, ss, ms)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms *([\+\-]?)$sign(1[0-2]|0?[0-9]|Z)$zh$$"                       => da(y, m, d, hh, mm, ss, ms, sign + zh)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$" => da(y, m, d, hh, mm, ss, ms, sign + zh, sign + zm)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss *([\+\-]?)$sign(1[0-2]|0?[0-9]|Z)$zh$$"                                     => da(y, m, d, hh, mm, ss, "0", sign + zh)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"               => da(y, m, d, hh, mm, ss, "0", sign + zh, sign + zm)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm *([\+\-]?)$sign(1[0-2]|0?[0-9]|Z)$zh$$"                                                             => da(y, m, d, hh, mm, "0", "0", sign + zh)
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"                                       => da(y, m, d, hh, mm, "0", "0", sign + zh, sign + zm)
      case other                                                                                                                                                                                                                     =>
        error(s"Unrecognized date pattern: `$other`")
    }

    new Date(milliSeconds)
  }

  def truncateDateStr(dateStr: String): String = {
    dateStr.trim match {
      case r"^(\d{1,4})$y$$"                                                                                                                                                                                                         => s"${p(y, 4)}-01-01"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m$$"                                                                                                                                                                                      => s"${p(y, 4)}-${p(m)}-01"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d$$"                                                                                                                                                          => s"${p(y, 4)}-${p(m)}-${p(d)}"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+00:00$$"                                                                                                                                               => s"${p(y, 4)}-${p(m)}-${p(d)}"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+00:00:00$$"                                                                                                                                            => s"${p(y, 4)}-${p(m)}-${p(d)}"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+00:00:00\.0{1,3}$$"                                                                                                                                    => s"${p(y, 4)}-${p(m)}-${p(d)}"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm$$"                                                                                                  => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:00$$"                                                                                               => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:00\.0{1,3}$$"                                                                                       => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss$$"                                                                          => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.0{1,3}$$"                                                                  => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms$$"                                                            => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}.$ms"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+00:00 *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"                                                                                    => s"${p(y, 4)}-${p(m)}-${p(d)}" + (if (local(sign, zh, zm)) "" else s" 00:00 $sign$zh:$zm")
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+00:00:00 *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"                                                                                 => s"${p(y, 4)}-${p(m)}-${p(d)}" + (if (local(sign, zh, zm)) "" else s" 00:00 $sign$zh:$zm")
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+00:00:00\.0{1,3} *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"                                                                         => s"${p(y, 4)}-${p(m)}-${p(d)}" + (if (local(sign, zh, zm)) "" else s" 00:00 $sign$zh:$zm")
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"                                       => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}" + (if (local(sign, zh, zm)) "" else s" $sign$zh:$zm")
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:00 *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"                                    => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}" + (if (local(sign, zh, zm)) "" else s" $sign$zh:$zm")
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:00\.0{1,3} *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"                            => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}" + (if (local(sign, zh, zm)) "" else s" $sign$zh:$zm")
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"               => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}" + (if (local(sign, zh, zm)) "" else s" $sign$zh:$zm")
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.0{1,3} *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"       => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}" + (if (local(sign, zh, zm)) "" else s" $sign$zh:$zm")
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$" => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}.$ms" + (if (local(sign, zh, zm)) "" else s" $sign$zh:$zm")


      case other =>
        error(s"Can't truncate unrecognized zoned date pattern: `$other`")
    }
  }

  def expandDateStr(dateStr: String): String = {
    dateStr.trim match {
      case r"^(\d{1,4})$y$$"                                                                                                                                                                                                         => s"${p(y, 4)}-01-01 00:00:00.000 $localOffset"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m$$"                                                                                                                                                                                      => s"${p(y, 4)}-${p(m)}-01 00:00:00.000 $localOffset"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d$$"                                                                                                                                                          => s"${p(y, 4)}-${p(m)}-${p(d)} 00:00:00.000 $localOffset"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh$$"                                                                                                                          => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:00:00.000 $localOffset"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm$$"                                                                                                  => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:00.000 $localOffset"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss$$"                                                                          => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}.000 $localOffset"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms$$"                                                            => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}.${p(ms, 3)} $localOffset"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss\.(\d{1,3})$ms *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$" => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}.${p(ms, 3)} $sign$zh:$zm"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm:([1-5][0-9]|0?[0-9])$ss *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"               => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:${p(ss)}.000 $sign$zh:$zm"
      case r"^(\d{1,4})$y-(1[0-2]|0?[0-9])$m-(3[01]|[12][0-9]|0?[0-9])$d['T ]+(2[0-3]|1[0-9]|0?[0-9])$hh:([1-5][0-9]|0?[0-9])$mm *([\+\-]?)$sign(1[0-2]|0?[0-9])$zh:([1-5][0-9]|0?[0-9])$zm$$"                                       => s"${p(y, 4)}-${p(m)}-${p(d)} ${p(hh)}:${p(mm)}:00.000 $sign$zh:$zm"
      case other                                                                                                                                                                                                                     =>
        error(s"Can't expand unrecognized date pattern: `$other`")
    }
  }
}
