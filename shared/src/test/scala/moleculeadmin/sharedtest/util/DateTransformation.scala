package moleculeadmin.sharedtest.util

import java.time.{OffsetDateTime, ZoneOffset}
import java.util.TimeZone
import moleculeadmin.shared.util.DateHandling
import utest._


object DateTransformation extends TestSuite with DateHandling {

  val plus1hourZone  = ZoneOffset.of("+1")
  val plus2hourZone  = ZoneOffset.of("+2")
  val minus1hourZone = ZoneOffset.of("-1")
  val minus2hourZone = ZoneOffset.of("-2")


  val tests = Tests {

    test("ZoneOffSet") {

      // Local time with time zone difference (if your computer is in that time zone!)
      dateLocal2str(str2dateLocal("2019-02-12")) ==> "2019-02-12 00:00:00 +02:00"

      // UTC default
      dateZone2str(str2date("2019-02-12")) ==> "2019-02-12"
    }

    test("Timezones") {

      val baseDateStr = "2019-02-12 23:00"

      // Create date from relaxed string and convert back to shortest str representation
      dateZone2str(str2date(baseDateStr)) ==> "2019-02-12 23:00:00"

      // Local hour is only 21
      dateZone2str(str2date(baseDateStr), minus2hourZone) ==> "2019-02-12 21:00:00 -02:00"

      // Date has already changed to the 13th
      dateZone2str(str2date(baseDateStr), plus2hourZone) ==> "2019-02-13 01:00:00 +02:00"

      // Month shifts
      dateZone2str(str2date("2019-05-31 23:00"), plus2hourZone) ==> "2019-06-01 01:00:00 +02:00"
      dateZone2str(str2date("2019-06-01 01:00"), minus2hourZone) ==> "2019-05-31 23:00:00 -02:00"

      // Year shifts
      dateZone2str(str2date("2018-12-31 23:00"), plus2hourZone) ==> "2019-01-01 01:00:00 +02:00"
      dateZone2str(str2date("2019-01-01 01:00"), minus2hourZone) ==> "2018-12-31 23:00:00 -02:00"


      // Create date from relaxed string and convert back to full str representation
      date2strFull(str2date(baseDateStr)) ==> "2019-02-12T23:00:00.000Z"

      // Local hour is only 21
      date2strFull(str2date(baseDateStr), minus2hourZone) ==> "2019-02-12T21:00:00.000-02:00"

      // Date has already changed to the 13th
      date2strFull(str2date(baseDateStr), plus2hourZone) ==> "2019-02-13T01:00:00.000+02:00"

      // Month shifts
      date2strFull(str2date("2019-05-31 23:00"), plus2hourZone) ==> "2019-06-01T01:00:00.000+02:00"
      date2strFull(str2date("2019-06-01 01:00"), minus2hourZone) ==> "2019-05-31T23:00:00.000-02:00"

      // Year shifts
      date2strFull(str2date("2018-12-31 23:00"), plus2hourZone) ==> "2019-01-01T01:00:00.000+02:00"
      date2strFull(str2date("2019-01-01 01:00"), minus2hourZone) ==> "2018-12-31T23:00:00.000-02:00"
    }


    test("Ancient years") {

      dateZone2str(str2date("2001")) ==> "2001-01-01"

      // Years before 1000 are padded with zeros
      dateZone2str(str2date("756")) ==> "0756-01-01"

      // Note: _not_ 1995, but 95AD!
      dateZone2str(str2date("95")) ==> "0095-01-01"

      // 1AD
      dateZone2str(str2date("1")) ==> "0001-01-01"
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
        "2001-1-1 0:0:0 Z",
        "2001-1-1 0:0:0 +0",
        "2001-1-1 0:0:0 +00",
        "2001-1-1 0:0:0 +0:0",
        "2001-1-1 0:0:0 +0:00",
        "2001-1-1 0:0:0 +00:00",
        "2001-1-1 0:0:0 -0",
        "2001-1-1 0:0:0 -00",
        "2001-1-1 0:0:0 -0:0",
        "2001-1-1 0:0:0 -0:00",
        "2001-1-1 0:0:0 -00:00",
        "2001-1-1 0:0:0.0",
        "2001-1-1 0:0:0.00",
        "2001-1-1 0:0:0.000",
        "2001-1-1 0:0:0.000 Z",
        "2001-1-1 0:0:0.000 +0",
        "2001-1-1 0:0:0.000 +00",
        "2001-1-1 0:0:0.000 +0:0",
        "2001-1-1 0:0:0.000 +0:00",
        "2001-1-1 0:0:0.000 +00:00",
        "2001-1-1 0:0:0.000 -0",
        "2001-1-1 0:0:0.000 -00",
        "2001-1-1 0:0:0.000 -0:0",
        "2001-1-1 0:0:0.000 -0:00",
        "2001-1-1 0:0:0.000 -00:00"
      ).foreach { str =>

        // Create date from relaxed string and convert back to shortest str representation
        dateZone2str(str2date(str)) ==> "2001-01-01"

        // UTC (London) time 1 hour ahead of Swedish time
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-01-01 01:00:00 +01:00"


        // Create date from relaxed string and convert back to full str representation
        date2strFull(str2date(str)) ==> "2001-01-01T00:00:00.000Z"

        // UTC (London) time 1 hour ahead of Swedish time
        date2strFull(str2date(str), plus1hourZone) ==> "2001-01-01T01:00:00.000+01:00"
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
        "2001-2-1 0:0:0 Z",
        "2001-2-1 0:0:0 +0",
        "2001-2-1 0:0:0 +00",
        "2001-2-1 0:0:0 +0:0",
        "2001-2-1 0:0:0 +0:00",
        "2001-2-1 0:0:0 +00:00",
        "2001-2-1 0:0:0 -0",
        "2001-2-1 0:0:0 -00",
        "2001-2-1 0:0:0 -0:0",
        "2001-2-1 0:0:0 -0:00",
        "2001-2-1 0:0:0 -00:00",
        "2001-2-1 0:0:0.0",
        "2001-2-1 0:0:0.00",
        "2001-2-1 0:0:0.000",
        "2001-2-1 0:0:0.000 Z",
        "2001-2-1 0:0:0.000 +0",
        "2001-2-1 0:0:0.000 +00",
        "2001-2-1 0:0:0.000 +0:0",
        "2001-2-1 0:0:0.000 +0:00",
        "2001-2-1 0:0:0.000 +00:00",
        "2001-2-1 0:0:0.000 -0",
        "2001-2-1 0:0:0.000 -00",
        "2001-2-1 0:0:0.000 -0:0",
        "2001-2-1 0:0:0.000 -0:00",
        "2001-2-1 0:0:0.000 -00:00"
      ).foreach { str =>

        // Create date from relaxed string and convert back to shortest str representation
        dateZone2str(str2date(str)) ==> "2001-02-01"

        // UTC (London) time 1 hour ahead of Swedish time
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-02-01 01:00:00 +01:00"


        // Create date from relaxed string and convert back to full str representation
        date2strFull(str2date(str)) ==> "2001-02-01T00:00:00.000Z"

        // UTC (London) time 1 hour ahead of Swedish time
        date2strFull(str2date(str), plus1hourZone) ==> "2001-02-01T01:00:00.000+01:00"
      }
    }


    test("y-m-d") {
      Seq(
        "2001-12-7",
        "2001-12-07",
        "2001-12-7 0:0",
        "2001-12-07 0:0",
        "2001-12-7 0:0:0",
        "2001-12-7 0:0:0 Z",
        "2001-12-7 0:0:0 +0",
        "2001-12-7 0:0:0 +00",
        "2001-12-7 0:0:0 +0:0",
        "2001-12-7 0:0:0 +0:00",
        "2001-12-7 0:0:0 +00:00",
        "2001-12-7 0:0:0 -0",
        "2001-12-7 0:0:0 -00",
        "2001-12-7 0:0:0 -0:0",
        "2001-12-7 0:0:0 -0:00",
        "2001-12-7 0:0:0 -00:00",
        "2001-12-7 0:0:0.0",
        "2001-12-7 0:0:0.00",
        "2001-12-7 0:0:0.000",
        "2001-12-7 0:0:0.000 Z",
        "2001-12-7 0:0:0.000 +0",
        "2001-12-7 0:0:0.000 +00",
        "2001-12-7 0:0:0.000 +0:0",
        "2001-12-7 0:0:0.000 +0:00",
        "2001-12-7 0:0:0.000 +00:00",
        "2001-12-7 0:0:0.000 -0",
        "2001-12-7 0:0:0.000 -00",
        "2001-12-7 0:0:0.000 -0:0",
        "2001-12-7 0:0:0.000 -0:00",
        "2001-12-7 0:0:0.000 -00:00"
      ).foreach { str =>
        //      println(str)

        // Create date from relaxed string and convert back to shortest str representation
        dateZone2str(str2date(str)) ==> "2001-12-07"

        // UTC (London) time 1 hour ahead of Swedish time
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-12-07 01:00:00 +01:00"


        // Create date from relaxed string and convert back to full str representation
        date2strFull(str2date(str)) ==> "2001-12-07T00:00:00.000Z"

        // UTC (London) time 1 hour ahead of Swedish time
        date2strFull(str2date(str), plus1hourZone) ==> "2001-12-07T01:00:00.000+01:00"
      }
    }


    test("y-m-d hh:mm") {
      Seq(
        "2001-12-7 8:6",
        "2001-12-7 8:06",
        "2001-12-7 08:6",
        "2001-12-7 08:06",
        "2001-12-7 08:06:0",
        "2001-12-7 08:06:0 Z",
        "2001-12-7 08:06:0 +0",
        "2001-12-7 08:06:0 +00",
        "2001-12-7 08:06:0 +0:0",
        "2001-12-7 08:06:0 +0:00",
        "2001-12-7 08:06:0 +00:00",
        "2001-12-7 08:06:0 -0",
        "2001-12-7 08:06:0 -00",
        "2001-12-7 08:06:0 -0:0",
        "2001-12-7 08:06:0 -0:00",
        "2001-12-7 08:06:0 -00:00",
        "2001-12-7 08:06:0.0",
        "2001-12-7 08:06:0.00",
        "2001-12-7 08:06:0.000",
        "2001-12-7 08:06:0.000 Z",
        "2001-12-7 08:06:0.000 +0",
        "2001-12-7 08:06:0.000 +00",
        "2001-12-7 08:06:0.000 +0:0",
        "2001-12-7 08:06:0.000 +0:00",
        "2001-12-7 08:06:0.000 +00:00",
        "2001-12-7 08:06:0.000 -0",
        "2001-12-7 08:06:0.000 -00",
        "2001-12-7 08:06:0.000 -0:0",
        "2001-12-7 08:06:0.000 -0:00",
        "2001-12-7 08:06:0.000 -00:00"
      ).foreach { str =>
        //      println(str)

        // Create date from relaxed string and convert back to shortest str representation
        dateZone2str(str2date(str)) ==> "2001-12-07 08:06:00"

        // UTC (London) time 1 hour ahead of Swedish time
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-12-07 09:06:00 +01:00"


        // Create date from relaxed string and convert back to full str representation
        date2strFull(str2date(str)) ==> "2001-12-07T08:06:00.000Z"

        // UTC (London) time 1 hour ahead of Swedish time
        date2strFull(str2date(str), plus1hourZone) ==> "2001-12-07T09:06:00.000+01:00"
      }
    }


    test("y-m-d hh:mm:ss") {
      Seq(
        "2001-7-14 15:48:2",
        "2001-7-14 15:48:02",
        "2001-7-14 15:48:02 Z",
        "2001-7-14 15:48:02 +0",
        "2001-7-14 15:48:02 +00",
        "2001-7-14 15:48:02 +0:0",
        "2001-7-14 15:48:02 +0:00",
        "2001-7-14 15:48:02 +00:00",
        "2001-7-14 15:48:02 -0",
        "2001-7-14 15:48:02 -00",
        "2001-7-14 15:48:02 -0:0",
        "2001-7-14 15:48:02 -0:00",
        "2001-7-14 15:48:02 -00:00",
        "2001-7-14 15:48:02.0",
        "2001-7-14 15:48:02.00",
        "2001-7-14 15:48:02.000",
        "2001-7-14 15:48:02.000 Z",
        "2001-7-14 15:48:02.000 +0",
        "2001-7-14 15:48:02.000 +00",
        "2001-7-14 15:48:02.000 +0:0",
        "2001-7-14 15:48:02.000 +0:00",
        "2001-7-14 15:48:02.000 +00:00",
        "2001-7-14 15:48:02.000 -0",
        "2001-7-14 15:48:02.000 -00",
        "2001-7-14 15:48:02.000 -0:0",
        "2001-7-14 15:48:02.000 -0:00",
        "2001-7-14 15:48:02.000 -00:00"
      ).foreach { str =>
        //      println(str)

        // Create date from relaxed string and convert back to shortest str representation
        dateZone2str(str2date(str)) ==> "2001-07-14 15:48:02"

        // UTC (London) time 1 hour ahead of Swedish time
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-07-14 16:48:02 +01:00"


        // Create date from relaxed string and convert back to full str representation
        date2strFull(str2date(str)) ==> "2001-07-14T15:48:02.000Z"

        // UTC (London) time 1 hour ahead of Swedish time
        date2strFull(str2date(str), plus1hourZone) ==> "2001-07-14T16:48:02.000+01:00"
      }
    }


    test("y-m-d hh:mm:ss.ms") {
      Seq(
        "2001-4-5 21:59:40.3",
        "2001-4-5 21:59:40.30",
        "2001-4-5 21:59:40.300",
        "2001-4-5 21:59:40.300 Z",
        "2001-4-5 21:59:40.300 +0",
        "2001-4-5 21:59:40.300 +00",
        "2001-4-5 21:59:40.300 +0:0",
        "2001-4-5 21:59:40.300 +0:00",
        "2001-4-5 21:59:40.300 +00:00",
        "2001-4-5 21:59:40.300 -0",
        "2001-4-5 21:59:40.300 -00",
        "2001-4-5 21:59:40.300 -0:0",
        "2001-4-5 21:59:40.300 -0:00",
        "2001-4-5 21:59:40.300 -00:00"
      ).foreach { str =>
        dateZone2str(str2date(str)) ==> "2001-04-05 21:59:40.300"
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-04-05 22:59:40.300 +01:00"
        date2strFull(str2date(str)) ==> "2001-04-05T21:59:40.300Z"
        date2strFull(str2date(str), plus1hourZone) ==> "2001-04-05T22:59:40.300+01:00"
      }

      Seq(
        "2001-4-5 21:59:40.03",
        "2001-4-5 21:59:40.030",
        "2001-4-5 21:59:40.030 Z",
        "2001-4-5 21:59:40.030 +0",
        "2001-4-5 21:59:40.030 +00",
        "2001-4-5 21:59:40.030 +0:0",
        "2001-4-5 21:59:40.030 +0:00",
        "2001-4-5 21:59:40.030 +00:00",
        "2001-4-5 21:59:40.030 -0",
        "2001-4-5 21:59:40.030 -00",
        "2001-4-5 21:59:40.030 -0:0",
        "2001-4-5 21:59:40.030 -0:00",
        "2001-4-5 21:59:40.030 -00:00"
      ).foreach { str =>
        dateZone2str(str2date(str)) ==> "2001-04-05 21:59:40.030"
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-04-05 22:59:40.030 +01:00"
        date2strFull(str2date(str)) ==> "2001-04-05T21:59:40.030Z"
        date2strFull(str2date(str), plus1hourZone) ==> "2001-04-05T22:59:40.030+01:00"
      }

      Seq(
        "2001-4-5 21:59:40.003",
        "2001-4-5 21:59:40.003 Z",
        "2001-4-5 21:59:40.003 +0",
        "2001-4-5 21:59:40.003 +00",
        "2001-4-5 21:59:40.003 +0:0",
        "2001-4-5 21:59:40.003 +0:00",
        "2001-4-5 21:59:40.003 +00:00",
        "2001-4-5 21:59:40.003 -0",
        "2001-4-5 21:59:40.003 -00",
        "2001-4-5 21:59:40.003 -0:0",
        "2001-4-5 21:59:40.003 -0:00",
        "2001-4-5 21:59:40.003 -00:00"
      ).foreach { str =>
        dateZone2str(str2date(str)) ==> "2001-04-05 21:59:40.003"
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-04-05 22:59:40.003 +01:00"
        date2strFull(str2date(str)) ==> "2001-04-05T21:59:40.003Z"
        date2strFull(str2date(str), plus1hourZone) ==> "2001-04-05T22:59:40.003+01:00"
      }
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

        // UTC time in London 5 hours later than New York
        dateZone2str(str2date(str)) ==> "2001-11-23 20:44:00"

        // Time in Sweden when time is 15:20 in New York (one more hour than London)
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-11-23 21:44:00 +01:00"


        // Create date from relaxed string and convert back to full str representation
        date2strFull(str2date(str)) ==> "2001-11-23T20:44:00.000Z"

        // UTC (London) time 1 hour ahead of Swedish time - OBS extra hour for daylights saving!
        date2strFull(str2date(str), plus1hourZone) ==> "2001-11-23T21:44:00.000+01:00"
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

        // UTC time in London 5 hours later than New York
        dateZone2str(str2date(str)) ==> "2001-11-23 20:44:33.768"

        // Time in Sweden when time is 15:20 in New York (one more hour than London)
        dateZone2str(str2date(str), plus1hourZone) ==> "2001-11-23 21:44:33.768 +01:00"


        // Create date from relaxed string and convert back to full str representation
        date2strFull(str2date(str)) ==> "2001-11-23T20:44:33.768Z"

        // UTC (London) time 1 hour ahead of Swedish time - OBS extra hour for daylights saving!
        date2strFull(str2date(str), plus1hourZone) ==> "2001-11-23T21:44:33.768+01:00"
      }
    }
  }
}