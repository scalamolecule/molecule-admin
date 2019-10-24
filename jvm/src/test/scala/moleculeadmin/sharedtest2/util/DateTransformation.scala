package moleculeadmin.sharedtest2.util

import java.time.ZoneOffset
import moleculeadmin.servertest.Adhoc.{date2str, expandDateStr, localOffset, str2date}
import molecule.util.DateHandling
import utest._


object DateTransformation extends TestSuite with DateHandling {

  val myPlus2hourZone  = ZoneOffset.of("+2")
  val plus1hourZone  = ZoneOffset.of("+1")
  val utcZone = ZoneOffset.of("+0") // Same as ZoneOffset.UTC
  val minus1hourZone = ZoneOffset.of("-1")

  val otherOffset = "+01:45"


  /*
    OBS: beware that all test here are based on being in a +2 timezone!
    todo: make independent of what timezone the tests are run in, maybe something like this:

    // always returns 2009-02-13T23:31:30
    val fixedClock = Clock.fixed(Instant.ofEpochSecond(1234567890L), ZoneOffset.ofHours(0))
    // fixedClock: java.time.Clock = FixedClock[2009-02-13T23:31:30Z,Z]

    val date = LocalDateTime.now(fixedClock)
  */

  val tests = Tests {

    test("truncateDateStr") {
      truncateDateStr(s"2019") ==> "2019-01-01"
      truncateDateStr(s"2019-02") ==> "2019-02-01"
      truncateDateStr(s"2019-02-13") ==> "2019-02-13"

      truncateDateStr(s"2019-02-13 00:00") ==> "2019-02-13"
      truncateDateStr(s"2019-02-13 00:00:00") ==> "2019-02-13"
      truncateDateStr(s"2019-02-13 00:00:00.000") ==> "2019-02-13"

      truncateDateStr(s"2019-02-13 00:17") ==> "2019-02-13 00:17"
      truncateDateStr(s"2019-02-13 14:00") ==> "2019-02-13 14:00"
      truncateDateStr(s"2019-02-13 14:17") ==> "2019-02-13 14:17"
      truncateDateStr(s"2019-02-13 14:17:00") ==> "2019-02-13 14:17"
      truncateDateStr(s"2019-02-13 14:17:00.000") ==> "2019-02-13 14:17"

      truncateDateStr(s"2019-02-13 14:17:56") ==> "2019-02-13 14:17:56"
      truncateDateStr(s"2019-02-13 14:17:56.000") ==> "2019-02-13 14:17:56"
      truncateDateStr(s"2019-02-13 14:17:56.876") ==> "2019-02-13 14:17:56.876"


      truncateDateStr(s"2019-02-12 00:00 $localOffset") ==> "2019-02-12"
      truncateDateStr(s"2019-02-12 00:00 $otherOffset") ==> s"2019-02-12 00:00 $otherOffset"

      truncateDateStr(s"2019-02-12 00:00:00 $localOffset") ==> "2019-02-12"
      truncateDateStr(s"2019-02-12 00:00:00 $otherOffset") ==> s"2019-02-12 00:00 $otherOffset"

      truncateDateStr(s"2019-02-12 00:00:00.000 $localOffset") ==> "2019-02-12"
      truncateDateStr(s"2019-02-12 00:00:00.000 $otherOffset") ==> s"2019-02-12 00:00 $otherOffset"

      truncateDateStr(s"2019-02-13 14:17 $localOffset") ==> s"2019-02-13 14:17"
      truncateDateStr(s"2019-02-13 14:17 $otherOffset") ==> s"2019-02-13 14:17 $otherOffset"

      truncateDateStr(s"2019-02-13 14:17:00 $localOffset") ==> s"2019-02-13 14:17"
      truncateDateStr(s"2019-02-13 14:17:00 $otherOffset") ==> s"2019-02-13 14:17 $otherOffset"

      truncateDateStr(s"2019-02-13 14:17:00.000 $localOffset") ==> s"2019-02-13 14:17"
      truncateDateStr(s"2019-02-13 14:17:00.000 $otherOffset") ==> s"2019-02-13 14:17 $otherOffset"

      truncateDateStr(s"2019-02-13 14:17:56 $localOffset") ==> s"2019-02-13 14:17:56"
      truncateDateStr(s"2019-02-13 14:17:56 $otherOffset") ==> s"2019-02-13 14:17:56 $otherOffset"

      truncateDateStr(s"2019-02-13 14:17:56.000 $localOffset") ==> s"2019-02-13 14:17:56"
      truncateDateStr(s"2019-02-13 14:17:56.000 $otherOffset") ==> s"2019-02-13 14:17:56 $otherOffset"

      truncateDateStr(s"2019-02-13 14:17:56.876 $localOffset") ==> s"2019-02-13 14:17:56.876"
      truncateDateStr(s"2019-02-13 14:17:56.876 $otherOffset") ==> s"2019-02-13 14:17:56.876 $otherOffset"
    }


    test("expandDateStr") {

      // Pre-millenial years (not abbreviations for 19xx/20xx)
      expandDateStr(s"2") ==> s"0002-01-01 00:00:00.000 $localOffset"
      expandDateStr(s"20") ==> s"0020-01-01 00:00:00.000 $localOffset"
      expandDateStr(s"201") ==> s"0201-01-01 00:00:00.000 $localOffset"

      expandDateStr(s"2019") ==> s"2019-01-01 00:00:00.000 $localOffset"

      expandDateStr(s"2019-2") ==> s"2019-02-01 00:00:00.000 $localOffset"

      expandDateStr(s"2019-2-1") ==> s"2019-02-01 00:00:00.000 $localOffset"
      expandDateStr(s"2019-02-13") ==> s"2019-02-13 00:00:00.000 $localOffset"

      expandDateStr(s"2019-02-13 0:0") ==> s"2019-02-13 00:00:00.000 $localOffset"
      expandDateStr(s"2019-02-13 1:1") ==> s"2019-02-13 01:01:00.000 $localOffset"
      expandDateStr(s"2019-02-13 14:1") ==> s"2019-02-13 14:01:00.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17") ==> s"2019-02-13 14:17:00.000 $localOffset"

      expandDateStr(s"2019-02-13 1:1:0") ==> s"2019-02-13 01:01:00.000 $localOffset"
      expandDateStr(s"2019-02-13 14:1:0") ==> s"2019-02-13 14:01:00.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:0") ==> s"2019-02-13 14:17:00.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:00") ==> s"2019-02-13 14:17:00.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:5") ==> s"2019-02-13 14:17:05.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56") ==> s"2019-02-13 14:17:56.000 $localOffset"

      expandDateStr(s"2019-02-13 14:17:56.0") ==> s"2019-02-13 14:17:56.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56.00") ==> s"2019-02-13 14:17:56.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56.000") ==> s"2019-02-13 14:17:56.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56.8") ==> s"2019-02-13 14:17:56.008 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56.87") ==> s"2019-02-13 14:17:56.087 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56.876") ==> s"2019-02-13 14:17:56.876 $localOffset"

      expandDateStr(s"2019-02-13 00:00 $localOffset") ==> s"2019-02-13 00:00:00.000 $localOffset"
      expandDateStr(s"2019-02-13 00:00 $otherOffset") ==> s"2019-02-13 00:00:00.000 $otherOffset"

      expandDateStr(s"2019-02-13 14:17 $localOffset") ==> s"2019-02-13 14:17:00.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17 $otherOffset") ==> s"2019-02-13 14:17:00.000 $otherOffset"

      expandDateStr(s"2019-02-13 14:17:0 $localOffset") ==> s"2019-02-13 14:17:00.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:0 $otherOffset") ==> s"2019-02-13 14:17:00.000 $otherOffset"
      expandDateStr(s"2019-02-13 14:17:00 $localOffset") ==> s"2019-02-13 14:17:00.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:00 $otherOffset") ==> s"2019-02-13 14:17:00.000 $otherOffset"

      expandDateStr(s"2019-02-13 14:17:5 $localOffset") ==> s"2019-02-13 14:17:05.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:5 $otherOffset") ==> s"2019-02-13 14:17:05.000 $otherOffset"
      expandDateStr(s"2019-02-13 14:17:56 $localOffset") ==> s"2019-02-13 14:17:56.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56 $otherOffset") ==> s"2019-02-13 14:17:56.000 $otherOffset"


      expandDateStr(s"2019-02-13 14:17:56.0 $localOffset") ==> s"2019-02-13 14:17:56.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56.00 $localOffset") ==> s"2019-02-13 14:17:56.000 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56.000 $localOffset") ==> s"2019-02-13 14:17:56.000 $localOffset"

      expandDateStr(s"2019-02-13 14:17:56.0 $otherOffset") ==> s"2019-02-13 14:17:56.000 $otherOffset"
      expandDateStr(s"2019-02-13 14:17:56.00 $otherOffset") ==> s"2019-02-13 14:17:56.000 $otherOffset"
      expandDateStr(s"2019-02-13 14:17:56.000 $otherOffset") ==> s"2019-02-13 14:17:56.000 $otherOffset"


      expandDateStr(s"2019-02-13 14:17:56.8 $localOffset") ==> s"2019-02-13 14:17:56.008 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56.87 $localOffset") ==> s"2019-02-13 14:17:56.087 $localOffset"
      expandDateStr(s"2019-02-13 14:17:56.876 $localOffset") ==> s"2019-02-13 14:17:56.876 $localOffset"

      expandDateStr(s"2019-02-13 14:17:56.8 $otherOffset") ==> s"2019-02-13 14:17:56.008 $otherOffset"
      expandDateStr(s"2019-02-13 14:17:56.87 $otherOffset") ==> s"2019-02-13 14:17:56.087 $otherOffset"
      expandDateStr(s"2019-02-13 14:17:56.876 $otherOffset") ==> s"2019-02-13 14:17:56.876 $otherOffset"
    }


    test("Pre-millenium years") {

      date2str(str2date("2001")) ==> "2001-01-01"

      // Years before 1000 are padded with zeros
      date2str(str2date("756")) ==> "0756-01-01"

      // Note: _not_ 1995, but 95AD!
      date2str(str2date("95")) ==> "0095-01-01"

      // 1AD
      date2str(str2date("1")) ==> "0001-01-01"
    }


    test("y") {
      Seq(
        "2001",
        "2001-1",
        "2001-01",
        "2001-1-1",
        "2001-1-01",
        "2001-01-1",
        "2001-01-01",
        "2001-1-1 0:0",
        "2001-1-1 0:0:0",
        "2001-1-1 0:0:0.0",
        "2001-1-1 0:0:0.00",
        "2001-1-1 0:0:0.000",
        "2001-1-1 00:00:00.000",
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-01-01"

        date2str(str2date(str), utcZone) ==> "2000-12-31 22:00 Z"
        date2str(str2date(str), plus1hourZone) ==> "2000-12-31 23:00 +01:00"
        date2str(str2date(str), minus1hourZone) ==> "2000-12-31 21:00 -01:00"
      }
    }


    test("y-m") {
      Seq(
        "2001-2",
        "2001-02",
        "2001-2-1",
        "2001-2-01",
        "2001-02-1",
        "2001-02-01",
        "2001-2-1 0:0",
        "2001-2-1 0:0:0",
        "2001-2-1 0:0:0.0",
        "2001-2-1 0:0:0.00",
        "2001-2-1 0:0:0.000",
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-02-01"
        date2str(str2date(str), plus1hourZone) ==> "2001-01-31 23:00 +01:00"
      }
    }


    test("y-m-d") {
      Seq(
        "2001-12-7",
        "2001-12-07",
        "2001-12-7 0:0",
        "2001-12-07 0:0",
        "2001-12-7 0:0:0",
        "2001-12-7 0:0:0.0",
        "2001-12-7 0:0:0.00",
        "2001-12-7 0:0:0.000",
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-12-07"
        date2str(str2date(str), plus1hourZone) ==> "2001-12-06 23:00 +01:00"
      }
    }


    test("y-m-d hh:mm") {
      Seq(
        "2001-12-7 8:6",
        "2001-12-7 8:06",
        "2001-12-7 08:6",
        "2001-12-7 08:06",
        "2001-12-7 08:06:0",
        "2001-12-7 08:06:0.0",
        "2001-12-7 08:06:0.00",
        "2001-12-7 08:06:0.000",
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-12-07 08:06"
        date2str(str2date(str), plus1hourZone) ==> "2001-12-07 07:06 +01:00"
      }
    }


    test("y-m-d hh:mm:ss") {
      Seq(
        "2001-7-14 15:48:2",
        "2001-7-14 15:48:02",
        "2001-7-14 15:48:02.0",
        "2001-7-14 15:48:02.00",
        "2001-7-14 15:48:02.000",
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-07-14 15:48:02"
        date2str(str2date(str), plus1hourZone) ==> "2001-07-14 14:48:02 +01:00"
      }
    }


    test("y-m-d hh:mm:ss.ms") {
      Seq(
        "2001-4-5 21:59:40.3",
        "2001-4-5 21:59:40.30",
        "2001-4-5 21:59:40.300",
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-04-05 21:59:40.300"
        date2str(str2date(str), plus1hourZone) ==> "2001-04-05 20:59:40.300 +01:00"
      }

      Seq(
        "2001-4-5 21:59:40.03",
        "2001-4-5 21:59:40.030",
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-04-05 21:59:40.030"
        date2str(str2date(str), plus1hourZone) ==> "2001-04-05 20:59:40.030 +01:00"
      }

      Seq(
        "2001-4-5 21:59:40.003",
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-04-05 21:59:40.003"
        date2str(str2date(str), plus1hourZone) ==> "2001-04-05 20:59:40.003 +01:00"
      }
    }


    test("Timezones") {

      val d1 = "2019-02-12 01:00"

      date2str(str2date(d1)) ==> "2019-02-12 01:00"
      date2str(str2date(d1), plus1hourZone) ==> "2019-02-12 00:00 +01:00"
      date2str(str2date(d1), utcZone) ==> "2019-02-11 23:00 Z"
      date2str(str2date(d1), minus1hourZone) ==> "2019-02-11 22:00 -01:00"


      val d2 = "2019-02-12 01:00 Z"

      date2str(str2date(d2)) ==> "2019-02-12 01:00"
      date2str(str2date(d2), plus1hourZone) ==> "2019-02-12 00:00 +01:00"
      date2str(str2date(d2), utcZone) ==> "2019-02-11 23:00 Z"
      date2str(str2date(d2), minus1hourZone) ==> "2019-02-11 22:00 -01:00"


      val d3 = "2019-02-12 01:00 +01:00"

      // Date is adjusted to current (+2) timezone
      date2str(str2date(d3)) ==> "2019-02-12 02:00"
      date2str(str2date(d3), myPlus2hourZone) ==> "2019-02-12 02:00"

      date2str(str2date(d3), plus1hourZone) ==> "2019-02-12 01:00 +01:00"
      date2str(str2date(d3), utcZone) ==> "2019-02-12 00:00 Z"
      date2str(str2date(d3), minus1hourZone) ==> "2019-02-11 23:00 -01:00"


      val d4 = "2019-02-12 01:00 -01:00"

      // Date is adjusted to current (+2) timezone
      date2str(str2date(d4)) ==> "2019-02-12 04:00"
      date2str(str2date(d4), myPlus2hourZone) ==> "2019-02-12 04:00"

      date2str(str2date(d4), plus1hourZone) ==> "2019-02-12 03:00 +01:00"
      date2str(str2date(d4), utcZone) ==> "2019-02-12 02:00 Z"
      date2str(str2date(d4), minus1hourZone) ==> "2019-02-12 01:00 -01:00"
    }


    test("y-m-d hh:mm:ss +z") {
      // Set standard time zone (without daylight savings) of New York (-5 hours)
      Seq(
        "2001-11-23 15:44:00 -5",
        "2001-11-23 15:44:00 -05",
        "2001-11-23 15:44:00 -05:0",
        "2001-11-23 15:44:00 -05:00",
        "2001-11-23 15:44:00 -05:00",

        // zero ms has no effect
        "2001-11-23 15:44:00.000 -5",
        "2001-11-23 15:44:00.000 -05",
        "2001-11-23 15:44:00.000 -5:0",
        "2001-11-23 15:44:00.000 -5:00",
        "2001-11-23 15:44:00.000 -05:00"
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-11-23 22:44"
        date2str(str2date(str), plus1hourZone) ==> "2001-11-23 21:44 +01:00"
      }
    }


    test("y-m-d hh:mm:ss.ms +z") {
      // Set standard time zone (without daylight savings) of New York (-5 hours)
      Seq(
        "2001-11-23 15:44:33.768 -5",
        "2001-11-23 15:44:33.768 -05",
        "2001-11-23 15:44:33.768 -5:0",
        "2001-11-23 15:44:33.768 -5:00",
        "2001-11-23 15:44:33.768 -05:00"
      ).foreach { str =>
        date2str(str2date(str)) ==> "2001-11-23 22:44:33.768"
        date2str(str2date(str), plus1hourZone) ==> "2001-11-23 21:44:33.768 +01:00"
      }
    }
  }
}