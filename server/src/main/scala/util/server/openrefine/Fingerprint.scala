package util

import java.util
import java.util.regex.Pattern
import org.apache.commons.lang.StringUtils

object Fingerprint {

  // Punctuation and control characters (except for TAB which we need for split to work)
  val punctctrl = Pattern.compile("\\p{Punct}|[\\x00-\\x08\\x0A-\\x1F\\x7F]")

  def fingerprint(s: String) = {
    val frags = StringUtils.split(
      asciify(
        punctctrl
          .matcher(
            s.trim.toLowerCase() // trim and lower input string
          ) // find special/control characters
          .replaceAll("") // remove special/control characters
      ) // find ASCII equivalent to characters
    ) // split by whitespace

    val set = new util.TreeSet[String]
    for (ss <- frags) {
      set.add(ss) // order fragments and dedupe
    }
    val b = new StringBuffer
    val i = set.iterator
    while (i.hasNext) { // join ordered fragments back together
      b.append(i.next)
      //    val x = "@" + i.next
      //      b.append(x)
      if (i.hasNext) b.append(' ')
    }
    b.toString
  }

  def asciify(s: String) = {
    val c = s.toCharArray
    val b = new StringBuffer
    for (element <- c) {
      b.append(translate(element))
    }
    b.toString
  }

  // Extended special characters!
  def translate(c: Char): String = {
    c match {
      case '\u00C0' => "a" // À
      case '\u00C1' => "a" // Á
      case '\u00C2' => "a" // Â
      case '\u00C3' => "a" // Ã
      case '\u00C4' => "a" // Ä
      case '\u00E0' => "a" // à
      case '\u00E1' => "a" // á
      case '\u00E2' => "a" // â
      case '\u00E3' => "a" // ã
      case '\u00E4' => "a" // ä
      case '\u0100' => "a" // Ā
      case '\u0101' => "a" // ā
      case '\u0102' => "a" // Ă
      case '\u0103' => "a" // ă
      case '\u0104' => "a" // Ą
      case '\u0105' => "a" // ą
      case '\u01CD' => "a" // Ǎ
      case '\u01CE' => "a" // ǎ
      case '\u01DE' => "a" // Ǟ
      case '\u01DF' => "a" // ǟ
      case '\u01E0' => "a" // Ǡ
      case '\u01E1' => "a" // ǡ
      case '\u01FA' => "a" // Ǻ
      case '\u01FB' => "a" // ǻ
      case '\u0200' => "a" // Ȁ
      case '\u0201' => "a" // ȁ
      case '\u0202' => "a" // Ȃ
      case '\u0203' => "a" // ȃ
      case '\u0226' => "a" // Ȧ
      case '\u0227' => "a" // ȧ
      case '\u023A' => "a" // Ⱥ

      case '\u00C5' => "aa" // Å
      case '\u00E5' => "aa" // å

      case '\u00C6' => "ae" // Æ
      case '\u00E6' => "ae" // æ
      case '\u01E2' => "ae" // Ǣ
      case '\u01E3' => "ae" // ǣ
      case '\u01FC' => "ae" // Ǽ
      case '\u01FD' => "ae" // ǽ

      case '\u0180' => "b" // ƀ
      case '\u0181' => "b" // Ɓ
      case '\u0182' => "b" // Ƃ
      case '\u0183' => "b" // ƃ
      case '\u0243' => "b" // Ƀ
      case '\u1E02' => "b" // Ḃ
      case '\u1E03' => "b" // ḃ

      case '\u00C7' => "c" // Ç
      case '\u00E7' => "c" // ç
      case '\u0106' => "c" // Ć
      case '\u0107' => "c" // ć
      case '\u0108' => "c" // Ĉ
      case '\u0109' => "c" // ĉ
      case '\u010A' => "c" // Ċ
      case '\u010B' => "c" // ċ
      case '\u010C' => "c" // Č
      case '\u010D' => "c" // č
      case '\u0187' => "c" // Ƈ
      case '\u0188' => "c" // ƈ
      case '\u023B' => "c" // Ȼ
      case '\u023C' => "c" // ȼ

      case '\u010E' => "d" // Ď
      case '\u010F' => "d" // ď
      case '\u0110' => "d" // Đ
      case '\u0111' => "d" // đ
      case '\u0189' => "d" // Ɖ
      case '\u018A' => "d" // Ɗ
      case '\u018B' => "d" // Ƌ
      case '\u018C' => "d" // ƌ
      case '\u018D' => "d" // ƍ
      case '\u0221' => "d" // ȡ
      case '\u1E0A' => "d" // Ḋ
      case '\u1E0B' => "d" // ḋ

      case '\u00C8' => "e" // È
      case '\u00C9' => "e" // É
      case '\u00CA' => "e" // Ê
      case '\u00CB' => "e" // Ë
      case '\u00E8' => "e" // è
      case '\u00E9' => "e" // é
      case '\u00EA' => "e" // ê
      case '\u00EB' => "e" // ë
      case '\u0112' => "e" // Ē
      case '\u0113' => "e" // ē
      case '\u0114' => "e" // Ĕ
      case '\u0115' => "e" // ĕ
      case '\u0116' => "e" // Ė
      case '\u0117' => "e" // ė
      case '\u0118' => "e" // Ę
      case '\u0119' => "e" // ę
      case '\u011A' => "e" // Ě
      case '\u011B' => "e" // ě
      case '\u018E' => "e" // Ǝ
      case '\u0190' => "e" // Ɛ
      case '\u01DD' => "e" // ǝ
      case '\u0204' => "e" // Ȅ
      case '\u0205' => "e" // ȅ
      case '\u0206' => "e" // Ȇ
      case '\u0207' => "e" // ȇ
      case '\u0228' => "e" // Ȩ
      case '\u0229' => "e" // ȩ
      case '\u0246' => "e" // Ɇ
      case '\u0247' => "e" // ɇ

      case '\u0191' => "f" // Ƒ
      case '\u0192' => "f" // ƒ
      case '\u1E1E' => "f" // Ḟ
      case '\u1E1F' => "f" // ḟ

      case '\u011C' => "g" // Ĝ
      case '\u011D' => "g" // ĝ
      case '\u011E' => "g" // Ğ
      case '\u011F' => "g" // ğ
      case '\u0120' => "g" // Ġ
      case '\u0121' => "g" // ġ
      case '\u0122' => "g" // Ģ
      case '\u0123' => "g" // ģ
      case '\u0193' => "g" // Ɠ
      case '\u0194' => "g" // Ɣ
      case '\u01E4' => "g" // Ǥ
      case '\u01E5' => "g" // ǥ
      case '\u01E6' => "g" // Ǧ
      case '\u01E7' => "g" // ǧ
      case '\u01F4' => "g" // Ǵ
      case '\u01F5' => "g" // ǵ

      case '\u0124' => "h" // Ĥ
      case '\u0125' => "h" // ĥ
      case '\u0126' => "h" // Ħ
      case '\u0127' => "h" // ħ
      case '\u021E' => "h" // Ȟ
      case '\u021F' => "h" // ȟ

      case '\u00CC' => "i" // Ì
      case '\u00CD' => "i" // Í
      case '\u00CE' => "i" // Î
      case '\u00CF' => "i" // Ï
      case '\u00EC' => "i" // ì
      case '\u00ED' => "i" // í
      case '\u00EE' => "i" // î
      case '\u00EF' => "i" // ï
      case '\u0128' => "i" // Ĩ
      case '\u0129' => "i" // ĩ
      case '\u012A' => "i" // Ī
      case '\u012B' => "i" // ī
      case '\u012C' => "i" // Ĭ
      case '\u012D' => "i" // ĭ
      case '\u012E' => "i" // Į
      case '\u012F' => "i" // į
      case '\u0130' => "i" // İ
      case '\u0131' => "i" // ı
      case '\u0196' => "i" // Ɩ
      case '\u0197' => "i" // Ɨ
      case '\u01CF' => "i" // Ǐ
      case '\u01D0' => "i" // ǐ
      case '\u0208' => "i" // Ȉ
      case '\u0209' => "i" // ȉ
      case '\u020A' => "i" // Ȋ
      case '\u020B' => "i" // ȋ

      case '\u0134' => "j" // Ĵ
      case '\u0135' => "j" // ĵ
      case '\u01F0' => "j" // ǰ
      case '\u0237' => "j" // ȷ
      case '\u0248' => "j" // Ɉ
      case '\u0249' => "j" // ɉ

      case '\u0136' => "k" // Ķ
      case '\u0137' => "k" // ķ
      case '\u0138' => "k" // ĸ
      case '\u0198' => "k" // Ƙ
      case '\u0199' => "k" // ƙ
      case '\u01E8' => "k" // Ǩ
      case '\u01E9' => "k" // ǩ

      case '\u0139' => "l" // Ĺ
      case '\u013A' => "l" // ĺ
      case '\u013B' => "l" // Ļ
      case '\u013C' => "l" // ļ
      case '\u013D' => "l" // Ľ
      case '\u013E' => "l" // ľ
      case '\u013F' => "l" // Ŀ
      case '\u0140' => "l" // ŀ
      case '\u0141' => "l" // Ł
      case '\u0142' => "l" // ł
      case '\u019A' => "l" // ƚ
      case '\u0234' => "l" // ȴ
      case '\u023D' => "l" // Ƚ

      case '\u019C' => "m" // Ɯ
      case '\u1E40' => "m" // Ṁ
      case '\u1E41' => "m" // ṁ

      case '\u00D1' => "n" // Ñ
      case '\u00F1' => "n" // ñ
      case '\u0143' => "n" // Ń
      case '\u0144' => "n" // ń
      case '\u0145' => "n" // Ņ
      case '\u0146' => "n" // ņ
      case '\u0147' => "n" // Ň
      case '\u0148' => "n" // ň
      case '\u014A' => "n" // Ŋ
      case '\u014B' => "n" // ŋ
      case '\u019D' => "n" // Ɲ
      case '\u019E' => "n" // ƞ
      case '\u01F8' => "n" // Ǹ
      case '\u01F9' => "n" // ǹ
      case '\u0220' => "n" // Ƞ
      case '\u0235' => "n" // ȵ

      case '\u00D2' => "o" // Ò
      case '\u00D3' => "o" // Ó
      case '\u00D4' => "o" // Ô
      case '\u00D5' => "o" // Õ
      case '\u00D6' => "o" // Ö
      case '\u00D8' => "o" // Ø
      case '\u00F2' => "o" // ò
      case '\u00F3' => "o" // ó
      case '\u00F4' => "o" // ô
      case '\u00F5' => "o" // õ
      case '\u00F6' => "o" // ö
      case '\u00F8' => "o" // ø
      case '\u014C' => "o" // Ō
      case '\u014D' => "o" // ō
      case '\u014E' => "o" // Ŏ
      case '\u014F' => "o" // ŏ
      case '\u0150' => "o" // Ő
      case '\u0151' => "o" // ő
      case '\u0186' => "o" // Ɔ
      case '\u019F' => "o" // Ɵ
      case '\u01A0' => "o" // Ơ
      case '\u01A1' => "o" // ơ
      case '\u01D1' => "o" // Ǒ
      case '\u01D2' => "o" // ǒ
      case '\u01EA' => "o" // Ǫ
      case '\u01EB' => "o" // ǫ
      case '\u01EC' => "o" // Ǭ
      case '\u01ED' => "o" // ǭ
      case '\u01FE' => "o" // Ǿ
      case '\u01FF' => "o" // ǿ
      case '\u020C' => "o" // Ȍ
      case '\u020D' => "o" // ȍ
      case '\u020E' => "o" // Ȏ
      case '\u020F' => "o" // ȏ
      case '\u022A' => "o" // Ȫ
      case '\u022B' => "o" // ȫ
      case '\u022C' => "o" // Ȭ
      case '\u022D' => "o" // ȭ
      case '\u022E' => "o" // Ȯ
      case '\u022F' => "o" // ȯ
      case '\u0230' => "o" // Ȱ
      case '\u0231' => "o" // ȱ

      case '\u0152' => "oe" // Œ
      case '\u0153' => "oe" // œ

      case '\u01A4' => "p" // Ƥ
      case '\u01A5' => "p" // ƥ
      case '\u1E56' => "p" // Ṗ
      case '\u1E57' => "p" // ṗ

      case '\u024A' => "q" // Ɋ
      case '\u024B' => "q" // ɋ

      case '\u0154' => "r" // Ŕ
      case '\u0155' => "r" // ŕ
      case '\u0156' => "r" // Ŗ
      case '\u0157' => "r" // ŗ
      case '\u0158' => "r" // Ř
      case '\u0159' => "r" // ř
      case '\u0210' => "r" // Ȑ
      case '\u0211' => "r" // ȑ
      case '\u0212' => "r" // Ȓ
      case '\u0213' => "r" // ȓ
      case '\u024C' => "r" // Ɍ
      case '\u024D' => "r" // ɍ

      case '\u015A' => "s" // Ś
      case '\u015B' => "s" // ś
      case '\u015C' => "s" // Ŝ
      case '\u015D' => "s" // ŝ
      case '\u015E' => "s" // Ş
      case '\u015F' => "s" // ş
      case '\u0160' => "s" // Š
      case '\u0161' => "s" // š
      case '\u017F' => "s" // ſ
      case '\u0218' => "s" // Ș
      case '\u0219' => "s" // ș
      case '\u023F' => "s" // ȿ
      case '\u1E60' => "s" // Ṡ
      case '\u1E61' => "s" // ṡ
      case '\u1E9B' => "s" // ẛ

      case '\u00DF' => "ss" // ß

      case '\u0162' => "t" // Ţ
      case '\u0163' => "t" // ţ
      case '\u0164' => "t" // Ť
      case '\u0165' => "t" // ť
      case '\u0166' => "t" // Ŧ
      case '\u0167' => "t" // ŧ
      case '\u01AB' => "t" // ƫ
      case '\u01AC' => "t" // Ƭ
      case '\u01AD' => "t" // ƭ
      case '\u01AE' => "t" // Ʈ
      case '\u021A' => "t" // Ț
      case '\u021B' => "t" // ț
      case '\u0236' => "t" // ȶ
      case '\u023E' => "t" // Ⱦ
      case '\u1E6A' => "t" // Ṫ
      case '\u1E6B' => "t" // ṫ

      case '\u00D9' => "u" // Ù
      case '\u00DA' => "u" // Ú
      case '\u00DB' => "u" // Û
      case '\u00DC' => "u" // Ü
      case '\u00F9' => "u" // ù
      case '\u00FA' => "u" // ú
      case '\u00FB' => "u" // û
      case '\u00FC' => "u" // ü
      case '\u0168' => "u" // Ũ
      case '\u0169' => "u" // ũ
      case '\u016A' => "u" // Ū
      case '\u016B' => "u" // ū
      case '\u016C' => "u" // Ŭ
      case '\u016D' => "u" // ŭ
      case '\u016E' => "u" // Ů
      case '\u016F' => "u" // ů
      case '\u0170' => "u" // Ű
      case '\u0171' => "u" // ű
      case '\u0172' => "u" // Ų
      case '\u0173' => "u" // ų
      case '\u01AF' => "u" // Ư
      case '\u01B0' => "u" // ư
      case '\u01D3' => "u" // Ǔ
      case '\u01D4' => "u" // ǔ
      case '\u01D5' => "u" // Ǖ
      case '\u01D6' => "u" // ǖ
      case '\u01D7' => "u" // Ǘ
      case '\u01D8' => "u" // ǘ
      case '\u01D9' => "u" // Ǚ
      case '\u01DA' => "u" // ǚ
      case '\u01DB' => "u" // Ǜ
      case '\u01DC' => "u" // ǜ
      case '\u0214' => "u" // Ȕ
      case '\u0215' => "u" // ȕ
      case '\u0216' => "u" // Ȗ
      case '\u0217' => "u" // ȗ
      case '\u0244' => "u" // Ʉ

      case '\u01B2' => "v" // Ʋ
      case '\u0245' => "v" // Ʌ

      case '\u0174' => "w" // Ŵ
      case '\u0175' => "w" // ŵ
      case '\u1E80' => "w" // Ẁ
      case '\u1E81' => "w" // ẁ
      case '\u1E82' => "w" // Ẃ
      case '\u1E83' => "w" // ẃ
      case '\u1E84' => "w" // Ẅ
      case '\u1E85' => "w" // ẅ

      case '\u00DD' => "y" // Ý
      case '\u00FD' => "y" // ý
      case '\u00FF' => "y" // ÿ
      case '\u0176' => "y" // Ŷ
      case '\u0177' => "y" // ŷ
      case '\u0178' => "y" // Ÿ
      case '\u01B3' => "y" // Ƴ
      case '\u01B4' => "y" // ƴ
      case '\u0232' => "y" // Ȳ
      case '\u0233' => "y" // ȳ
      case '\u024E' => "y" // Ɏ
      case '\u024F' => "y" // ɏ
      case '\u1EF2' => "y" // Ỳ
      case '\u1EF3' => "y" // ỳ

      case '\u0179' => "z" // Ź
      case '\u017A' => "z" // ź
      case '\u017B' => "z" // Ż
      case '\u017C' => "z" // ż
      case '\u017D' => "z" // Ž
      case '\u017E' => "z" // ž
      case '\u01B5' => "z" // Ƶ
      case '\u01B6' => "z" // ƶ
      case '\u0224' => "z" // Ȥ
      case '\u0225' => "z" // ȥ
      case '\u0240' => "z" // ɀ

      case other => other.toString
    }
  }


  /**
    * Translate the given unicode char in the closest ASCII representation
    * NOTE: this function deals only with latin-1 supplement and latin-1 extended code charts
    */
  private def translateOLD(c: Char) = {
    c match {
      case '\u00C0' => 'a'
      case '\u00C1' => 'a'
      case '\u00C2' => 'a'
      case '\u00C3' => 'a'
      case '\u00C4' => 'a'
      case '\u00C5' => 'a'
      case '\u00E0' => 'a'
      case '\u00E1' => 'a'
      case '\u00E2' => 'a'
      case '\u00E3' => 'a'
      case '\u00E4' => 'a'
      case '\u00E5' => 'a'
      case '\u0100' => 'a'
      case '\u0101' => 'a'
      case '\u0102' => 'a'
      case '\u0103' => 'a'
      case '\u0104' => 'a'
      case '\u0105' => 'a'

      case '\u00C7' => 'c'
      case '\u00E7' => 'c'
      case '\u0106' => 'c'
      case '\u0107' => 'c'
      case '\u0108' => 'c'
      case '\u0109' => 'c'
      case '\u010A' => 'c'
      case '\u010B' => 'c'
      case '\u010C' => 'c'
      case '\u010D' => 'c'

      case '\u00D0' => 'd'
      case '\u00F0' => 'd'
      case '\u010E' => 'd'
      case '\u010F' => 'd'
      case '\u0110' => 'd'
      case '\u0111' => 'd'

      case '\u00C8' => 'e'
      case '\u00C9' => 'e'
      case '\u00CA' => 'e'
      case '\u00CB' => 'e'
      case '\u00E8' => 'e'
      case '\u00E9' => 'e'
      case '\u00EA' => 'e'
      case '\u00EB' => 'e'
      case '\u0112' => 'e'
      case '\u0113' => 'e'
      case '\u0114' => 'e'
      case '\u0115' => 'e'
      case '\u0116' => 'e'
      case '\u0117' => 'e'
      case '\u0118' => 'e'
      case '\u0119' => 'e'
      case '\u011A' => 'e'
      case '\u011B' => 'e'


      case '\u011C' => 'g'
      case '\u011D' => 'g'
      case '\u011E' => 'g'
      case '\u011F' => 'g'
      case '\u0120' => 'g'
      case '\u0121' => 'g'
      case '\u0122' => 'g'
      case '\u0123' => 'g'

      case '\u0124' => 'h'
      case '\u0125' => 'h'
      case '\u0126' => 'h'
      case '\u0127' => 'h'

      case '\u00CC' => 'i'
      case '\u00CD' => 'i'
      case '\u00CE' => 'i'
      case '\u00CF' => 'i'
      case '\u00EC' => 'i'
      case '\u00ED' => 'i'
      case '\u00EE' => 'i'
      case '\u00EF' => 'i'
      case '\u0128' => 'i'
      case '\u0129' => 'i'
      case '\u012A' => 'i'
      case '\u012B' => 'i'
      case '\u012C' => 'i'
      case '\u012D' => 'i'
      case '\u012E' => 'i'
      case '\u012F' => 'i'
      case '\u0130' => 'i'
      case '\u0131' => 'i'

      case '\u0134' => 'j'
      case '\u0135' => 'j'

      case '\u0136' => 'k'
      case '\u0137' => 'k'
      case '\u0138' => 'k'

      case '\u0139' => 'l'
      case '\u013A' => 'l'
      case '\u013B' => 'l'
      case '\u013C' => 'l'
      case '\u013D' => 'l'
      case '\u013E' => 'l'
      case '\u013F' => 'l'
      case '\u0140' => 'l'
      case '\u0141' => 'l'
      case '\u0142' => 'l'

      case '\u00D1' => 'n'
      case '\u00F1' => 'n'
      case '\u0143' => 'n'
      case '\u0144' => 'n'
      case '\u0145' => 'n'
      case '\u0146' => 'n'
      case '\u0147' => 'n'
      case '\u0148' => 'n'
      case '\u0149' => 'n'
      case '\u014A' => 'n'
      case '\u014B' => 'n'

      case '\u00D2' => 'o'
      case '\u00D3' => 'o'
      case '\u00D4' => 'o'
      case '\u00D5' => 'o'
      case '\u00D6' => 'o'
      case '\u00D8' => 'o'
      case '\u00F2' => 'o'
      case '\u00F3' => 'o'
      case '\u00F4' => 'o'
      case '\u00F5' => 'o'
      case '\u00F6' => 'o'
      case '\u00F8' => 'o'
      case '\u014C' => 'o'
      case '\u014D' => 'o'
      case '\u014E' => 'o'
      case '\u014F' => 'o'
      case '\u0150' => 'o'
      case '\u0151' => 'o'

      case '\u0154' => 'r'
      case '\u0155' => 'r'
      case '\u0156' => 'r'
      case '\u0157' => 'r'
      case '\u0158' => 'r'
      case '\u0159' => 'r'

      case '\u015A' => 's'
      case '\u015B' => 's'
      case '\u015C' => 's'
      case '\u015D' => 's'
      case '\u015E' => 's'
      case '\u015F' => 's'
      case '\u0160' => 's'
      case '\u0161' => 's'
      case '\u017F' => 's'

      case '\u0162' => 't'
      case '\u0163' => 't'
      case '\u0164' => 't'
      case '\u0165' => 't'
      case '\u0166' => 't'
      case '\u0167' => 't'

      case '\u00D9' => 'u'
      case '\u00DA' => 'u'
      case '\u00DB' => 'u'
      case '\u00DC' => 'u'
      case '\u00F9' => 'u'
      case '\u00FA' => 'u'
      case '\u00FB' => 'u'
      case '\u00FC' => 'u'
      case '\u0168' => 'u'
      case '\u0169' => 'u'
      case '\u016A' => 'u'
      case '\u016B' => 'u'
      case '\u016C' => 'u'
      case '\u016D' => 'u'
      case '\u016E' => 'u'
      case '\u016F' => 'u'
      case '\u0170' => 'u'
      case '\u0171' => 'u'
      case '\u0172' => 'u'
      case '\u0173' => 'u'

      case '\u0174' => 'w'
      case '\u0175' => 'w'

      case '\u00DD' => 'y'
      case '\u00FD' => 'y'
      case '\u00FF' => 'y'
      case '\u0176' => 'y'
      case '\u0177' => 'y'
      case '\u0178' => 'y'

      case '\u0179' => 'z'
      case '\u017A' => 'z'
      case '\u017B' => 'z'
      case '\u017C' => 'z'
      case '\u017D' => 'z'
      case '\u017E' => 'z'

      case other => other
    }
  }


}
