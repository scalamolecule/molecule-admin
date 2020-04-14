//package util
//
////import org.apache.commons.lang.StringUtils
//import org.apache.commons.lang.StringUtils
//
//
//// TODO: Needs work...
//object NGram {
//
//  def apply(s: String, count: Int): Seq[String] = {
//    val tokens = StringUtils.split(s)
//    if (count >= tokens.length) return Array[String](s)
//    val len    = tokens.length - count + 1
//    val ngrams = new Array[String](len)
//    var i      = 0
//    while (i < len) {
//      val ss = new Array[String](count)
//      var j  = 0
//      while (j < count) {
//        ss(j) = tokens(i + j)
//        j += 1
//        j - 1
//      }
//      ngrams(i) = StringUtils.join(ss, ' ')
//      i += 1
//      i - 1
//    }
//    ngrams.toSeq
//  }
//
//
//}
