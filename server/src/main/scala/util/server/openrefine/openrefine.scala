package util.server.openrefine
import com.google.refine.clustering.binning._
import util.{Fingerprint, NGram}



object openrefine {

  object fingerprint {
    // Copy with expanded special character list
    def apply(s: String): String = Fingerprint.fingerprint(s)
  }


  // TODO: Not ready yet...
  object ngram {
    def apply(s: String, count: Int): Seq[String] = NGram(s, count)
  }


  object ngramFingerprint {
    lazy val keyer = new NGramFingerprintKeyer()
    def apply(s: String): String = keyer.key(s, null)
  }


  object phonetic {
    lazy val colognePhoneticKeyer = new ColognePhoneticKeyer()
    def cologne(s: String): String = colognePhoneticKeyer.key(s, null)

    lazy val doubleMetaphoneKeyer = new DoubleMetaphoneKeyer()
    def doubleMetaphone(s: String): String = doubleMetaphoneKeyer.key(s, null)

    lazy val metaphoneKeyer = new MetaphoneKeyer()
    def metaphone(s: String): String = metaphoneKeyer.key(s, null)

    lazy val metaphone3Keyer = new Metaphone3Keyer()
    def metaphone3(s: String): String = metaphone3Keyer.key(s, null)

    lazy val soundexKeyer = new SoundexKeyer()
    def soundex(s: String): String = soundexKeyer.key(s, null)
  }

}
