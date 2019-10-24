package moleculeadmin.shared.testdata

import java.net.URI
import java.util.Date
import java.util.UUID._
import molecule.util.DateHandling


trait ExampleData extends DateHandling {

  def da(i: Int): Date = {
    // Alternate between winter/summer time to test daylight savings too
    val month = i % 2 * 6 + 1
    str2date((2000 + i).toString + "-" + month)
  }
  def uu = randomUUID()
  def ur(i: Int) = new URI("uri" + i)

  val List(date0, date1, date2, date3, date4) = List(da(0), da(1), da(2), da(3), da(4))
  val List(uuid0, uuid1, uuid2, uuid3) = List(uu, uu, uu, uu).sortBy(_.toString)
  val List(uri0, uri1, uri2, uri3)     = List(ur(0), ur(1), ur(2), ur(3))

  val (str0, int0, long0, float0, double0, bigInt0, bigDec0, bool0, enum0, byte0) = (" ", 0, 0L, 0.0f, 0.0, BigInt(0), BigDecimal(0.1), false, "enum0", 0.toByte)
  val (str1, int1, long1, float1, double1, bigInt1, bigDec1, bool1, enum1, byte1) = ("a", 1, 1L, 1.0f, 1.0, BigInt(1), BigDecimal(1.1), true, "enum1", 1.toByte)
  val (str2, int2, long2, float2, double2, bigInt2, bigDec2, bool2, enum2, byte2) = ("b", 2, 2L, 2.0f, 2.0, BigInt(2), BigDecimal(2.2), false, "enum2", 2.toByte)
  val (str3, int3, long3, float3, double3, bigInt3, bigDec3, bool3, enum3, byte3) = ("c", 3, 3L, 3.0f, 3.0, BigInt(3), BigDecimal(3.3), true, "enum3", 3.toByte)


  val (strs0, ints0, longs0, floats0, doubles0, bools0, dates0, uuids0, uris0, enums0, bytes0) = (
    Set(str0),
    Set(int0),
    Set(long0),
    Set(float0),
    Set(double0),
    Set(bool0),
    Set(date0),
    Set(uuid0),
    Set(uri0),
    Set(enum0),
    Set(byte0)
  )

  val (strs1, ints1, longs1, floats1, doubles1, bools1, dates1, uuids1, uris1, enums1, bytes1) = (
    Set(str1),
    Set(int1),
    Set(long1),
    Set(float1),
    Set(double1),
    Set(bool1),
    Set(date1),
    Set(uuid1),
    Set(uri1),
    Set(enum1),
    Set(byte1)
  )

  val (strs2, ints2, longs2, floats2, doubles2, bools2, dates2, uuids2, uris2, enums2, bytes2) = (
    Set(str1, str2),
    Set(int1, int2),
    Set(long1, long2),
    Set(float1, float2),
    Set(double1, double2),
    Set(bool1, bool2),
    Set(date1, date2),
    Set(uuid1, uuid2),
    Set(uri1, uri2),
    Set(enum1, enum2),
    Set(byte1, byte2)
  )
}