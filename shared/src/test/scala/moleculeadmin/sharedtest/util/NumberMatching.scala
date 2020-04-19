package moleculeadmin.sharedtest.util

import molecule.util.RegexMatching
import utest.{TestSuite, Tests, test}

object NumberMatching extends TestSuite with RegexMatching {

  val numbers = Seq(
    Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve",
      "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "twenty-one", "twenty-two", "twenty-three", "twenty-four"),

    Seq("ein", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun", "zehn", "elf", "zwölf",
      "dreizehn", "vierzehn", "funfzehn", "sechszehn", "siebzehn", "achtzehn", "neunzehn", "zwanzig", "einundzwanzig", "zweiundzwanzig", "dreiundzwanzig", "vierundzwanzig"),

    Seq("un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix", "onze", "douze",
      "treize", "quatorze", "quinze", "seize", "dix-sept", "dix-huit", "dix-neuf", "vingt", "vingtet un", "vingt-deux", "vingt-trois", "vingt-quatre"),

    Seq("uno", "due", "tre", "quattro", "cinque", "sei", "sette", "otto", "nove", "dieci", "undici", "dodici",
      "tredici", "quattordici", "quindici", "sedici", "diciassette", "diciotto", "diciannove", "venti", "ventuno", "ventidue", "ventitre", "ventiquattro"),

    Seq("uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "once", "doce",
      "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve", "veinte", "veintiuno", "veintidós", "veintitrés", "veinticuatro"),

    // Dutch
    Seq("een", "twee", "drie", "vier", "vijf", "zes", "zeven", "acht", "negen", "tien", "elf", "twaalf",
      "dertien", "veertien", "vijftien", "zestien", "zeventien", "achttien", "negentien", "twintig", "eenentwintig", "tweeëntwintig", "drieëntwintig", "vierentwintig"),

    // Portuguise
    Seq("um", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove", "dez", "onze", "doze",
      "treze", "catorze", "quinze", "dezasseis", "dezassete", "dezoito", "dezanove", "vinte", "vintee um", "vintee dois", "vintee três", "vintee quatro"),

    // Polish
    Seq("jeden", "dwa", "trzy", "cztery", "piec", "szesc", "siedem", "osiem", "dziewiec", "dziesiec", "jedenascie", "dwanascie",
      "trzynascie", "czternascie", "pietnascie", "szesnascie", "siedemnascie", "osiemnascie", "dziewietnascie", "dwadziescia", "dwadziesciajeden", "dwadziesciadwa", "dwadziesciatrzy", "dwadziesciacztery"),

    // Romanian
    Seq("unu", "doi", "trei", "patru", "cinci", "sase", "sapte", "opt", "noua", "zece", "unsprezece", "doisprezece",
      "treisprezece", "paisprezece", "cincisprezece", "saisprezece", "saptesprezece", "optsprezece", "nouasprezece", "douazeci", "douazecisiunu", "douazecisidoi", "douazecisitrei", "douazecisipatru"),

    Seq("en|et", "to", "tre", "fire", "fem", "seks", "syv", "otte", "ni", "ti", "elleve", "tolv",
      "tretten", "fjorten", "femten", "seksten", "sytten", "atten", "nitten", "tyve", "enogtyve", "toogtyve", "treogtyve", "fireogtyve"),

    Seq("en|ett", "två", "tre", "fyra", "fem", "sex", "sju", "otta", "nio", "tio", "elva", "tolv",
      "tretton", "fjorton", "femton", "sexton", "sjutton", "arton", "nitton", "tjugo", "tjugoen", "tjugotvå", "tjugotre", "tjugofyra"),

    Seq("en|ett", "to", "tre", "fire", "fem", "seks", "sju|syv", "åtte", "ni", "ti", "elleve", "tolv",
      "tretten", "fjorten", "femten", "seksten", "sytten", "atten", "nitten", "tjue", "tjueen", "tjueto", "tjuetre", "tjuefire"),

    Seq("yksi", "kaksi", "kolme", "neljä", "viisi", "kuusi", "seitsemän", "kahdeksan", "yhdeksän", "kymmenen", "yksitoista", "kaksitoista",
      "kolmetoista", "neljätoista", "viisitoista", "kuusitoista", "seitsemäntoista", "kahdeksantoista", "yhdeksäntoista", "kaksikymmentä", "kaksikymmentäyksi", "kaksikymmentäkaksi", "kaksikymmentäkolme", "kaksikymmentäneljä"),
  )


  val tests = Tests {

    test("all") {

      val regexFlat = numbers.flatten.distinct.mkString("(", "|", ")")
      //      println(regexFlat)


      val regexLines = numbers.map(_.mkString("|")).mkString("(\n", "|\n", "\n)")
      //      println(regexLines)

      //      val regexLines2_ = numbers.map(_.drop(1).mkString("|")).mkString("(\n", "|\n", "\n)")
      //      println(regexLines2_)


    }

    test("transposet") {
      val numbersTransposed = numbers.transpose

      val cases = numbersTransposed.zipWithIndex.map { case (regexes, i) =>
        regexes.distinct.mkString("case \"", "\" | \"", "\" => " + (i + 1))
      }
      //      cases foreach println

      val cases2 = numbersTransposed.zipWithIndex.map { case (regexes, i) =>
        //        regexes.distinct.mkString("case r\"(?i)(", "|", ")$x (.*)$z\" => " + (i + 1))
        regexes.distinct.mkString("case r\"(?i)(", "|", ")$x (.*)$s\" => s")
      }
      cases2 foreach println


      //      "" match {
      //        case "one" | "ein" | "un" | "uno" | "een" | "um" | "jeden" | "unu" | "en|et" | "en|ett" | "yksi"                                                                                                                                        => 1
      //        case "two" | "zwei" | "deux" | "due" | "dos" | "twee" | "dois" | "dwa" | "doi" | "to" | "två" | "kaksi"                                                                                                                                 => 2
      //        case "three" | "drei" | "trois" | "tre" | "tres" | "drie" | "três" | "trzy" | "trei" | "kolme"                                                                                                                                          => 3
      //        case "four" | "vier" | "quatre" | "quattro" | "cuatro" | "quatro" | "cztery" | "patru" | "fire" | "fyra" | "neljä"                                                                                                                      => 4
      //        case "five" | "fünf" | "cinq" | "cinque" | "cinco" | "vijf" | "piec" | "cinci" | "fem" | "viisi"                                                                                                                                        => 5
      //        case "six" | "sechs" | "sei" | "seis" | "zes" | "szesc" | "sase" | "seks" | "sex" | "kuusi"                                                                                                                                             => 6
      //        case "seven" | "sieben" | "sept" | "sette" | "siete" | "zeven" | "sete" | "siedem" | "sapte" | "syv" | "sju" | "sju|syv" | "seitsemän"                                                                                                  => 7
      //        case "eight" | "acht" | "huit" | "otto" | "ocho" | "oito" | "osiem" | "opt" | "otte" | "otta" | "åtte" | "kahdeksan"                                                                                                                    => 8
      //        case "nine" | "neun" | "neuf" | "nove" | "nueve" | "negen" | "dziewiec" | "noua" | "ni" | "nio" | "yhdeksän"                                                                                                                            => 9
      //        case "ten" | "zehn" | "dix" | "dieci" | "diez" | "tien" | "dez" | "dziesiec" | "zece" | "ti" | "tio" | "kymmenen"                                                                                                                       => 10
      //        case "eleven" | "elf" | "onze" | "undici" | "once" | "jedenascie" | "unsprezece" | "elleve" | "elva" | "yksitoista"                                                                                                                     => 11
      //        case "twelve" | "zwölf" | "douze" | "dodici" | "doce" | "twaalf" | "doze" | "dwanascie" | "doisprezece" | "tolv" | "kaksitoista"                                                                                                        => 12
      //        case "thirteen" | "dreizehn" | "treize" | "tredici" | "trece" | "dertien" | "treze" | "trzynascie" | "treisprezece" | "tretten" | "tretton" | "kolmetoista"                                                                             => 13
      //        case "fourteen" | "vierzehn" | "quatorze" | "quattordici" | "catorce" | "veertien" | "catorze" | "czternascie" | "paisprezece" | "fjorten" | "fjorton" | "neljätoista"                                                                  => 14
      //        case "fifteen" | "funfzehn" | "quinze" | "quindici" | "quince" | "vijftien" | "pietnascie" | "cincisprezece" | "femten" | "femton" | "viisitoista"                                                                                      => 15
      //        case "sixteen" | "sechszehn" | "seize" | "sedici" | "dieciséis" | "zestien" | "dezasseis" | "szesnascie" | "saisprezece" | "seksten" | "sexton" | "kuusitoista"                                                                         => 16
      //        case "seventeen" | "siebzehn" | "dix-sept" | "diciassette" | "diecisiete" | "zeventien" | "dezassete" | "siedemnascie" | "saptesprezece" | "sytten" | "sjutton" | "seitsemäntoista"                                                     => 17
      //        case "eighteen" | "achtzehn" | "dix-huit" | "diciotto" | "dieciocho" | "achttien" | "dezoito" | "osiemnascie" | "optsprezece" | "atten" | "arton" | "kahdeksantoista"                                                                   => 18
      //        case "nineteen" | "neunzehn" | "dix-neuf" | "diciannove" | "diecinueve" | "negentien" | "dezanove" | "dziewietnascie" | "nouasprezece" | "nitten" | "nitton" | "yhdeksäntoista"                                                         => 19
      //        case "twenty" | "zwanzig" | "vingt" | "venti" | "veinte" | "twintig" | "vinte" | "dwadziescia" | "douazeci" | "tyve" | "tjugo" | "tjue" | "kaksikymmentä"                                                                               => 20
      //        case "twenty-one" | "einundzwanzig" | "vingtet un" | "ventuno" | "veintiuno" | "eenentwintig" | "vintee um" | "dwadziesciajeden" | "douazecisiunu" | "enogtyve" | "tjugoen" | "tjueen" | "kaksikymmentäyksi"                            => 21
      //        case "twenty-two" | "zweiundzwanzig" | "vingt-deux" | "ventidue" | "veintidós" | "tweeëntwintig" | "vintee dois" | "dwadziesciadwa" | "douazecisidoi" | "toogtyve" | "tjugotvå" | "tjueto" | "kaksikymmentäkaksi"                       => 22
      //        case "twenty-three" | "dreiundzwanzig" | "vingt-trois" | "ventitre" | "veintitrés" | "drieëntwintig" | "vintee três" | "dwadziesciatrzy" | "douazecisitrei" | "treogtyve" | "tjugotre" | "tjuetre" | "kaksikymmentäkolme"               => 23
      //        case "twenty-four" | "vierundzwanzig" | "vingt-quatre" | "ventiquattro" | "veinticuatro" | "vierentwintig" | "vintee quatro" | "dwadziesciacztery" | "douazecisipatru" | "fireogtyve" | "tjugofyra" | "tjuefire" | "kaksikymmentäneljä" => 24
      //      }
      //
      //      "" match {
      //        case r"(?i)(one|ein|un|uno|een|um|jeden|unu|en|et|en|ett|yksi) (.*)"                                                                                                                                => 1
      //        case r"(?i)(two|zwei|deux|due|dos|twee|dois|dwa|doi|to|två|kaksi) (.*)"                                                                                                                             => 2
      //        case r"(?i)(three|drei|trois|tre|tres|drie|três|trzy|trei|kolme) (.*)"                                                                                                                              => 3
      //        case r"(?i)(four|vier|quatre|quattro|cuatro|quatro|cztery|patru|fire|fyra|neljä) (.*)"                                                                                                              => 4
      //        case r"(?i)(five|fünf|cinq|cinque|cinco|vijf|piec|cinci|fem|viisi) (.*)"                                                                                                                            => 5
      //        case r"(?i)(six|sechs|sei|seis|zes|szesc|sase|seks|sex|kuusi) (.*)"                                                                                                                                 => 6
      //        case r"(?i)(seven|sieben|sept|sette|siete|zeven|sete|siedem|sapte|syv|sju|sju|syv|seitsemän) (.*)"                                                                                                  => 7
      //        case r"(?i)(eight|acht|huit|otto|ocho|oito|osiem|opt|otte|otta|åtte|kahdeksan) (.*)"                                                                                                                => 8
      //        case r"(?i)(nine|neun|neuf|nove|nueve|negen|dziewiec|noua|ni|nio|yhdeksän) (.*)"                                                                                                                    => 9
      //        case r"(?i)(ten|zehn|dix|dieci|diez|tien|dez|dziesiec|zece|ti|tio|kymmenen) (.*)"                                                                                                                   => 10
      //        case r"(?i)(eleven|elf|onze|undici|once|jedenascie|unsprezece|elleve|elva|yksitoista) (.*)"                                                                                                         => 11
      //        case r"(?i)(twelve|zwölf|douze|dodici|doce|twaalf|doze|dwanascie|doisprezece|tolv|kaksitoista) (.*)"                                                                                                => 12
      //        case r"(?i)(thirteen|dreizehn|treize|tredici|trece|dertien|treze|trzynascie|treisprezece|tretten|tretton|kolmetoista) (.*)"                                                                         => 13
      //        case r"(?i)(fourteen|vierzehn|quatorze|quattordici|catorce|veertien|catorze|czternascie|paisprezece|fjorten|fjorton|neljätoista) (.*)"                                                              => 14
      //        case r"(?i)(fifteen|funfzehn|quinze|quindici|quince|vijftien|pietnascie|cincisprezece|femten|femton|viisitoista) (.*)"                                                                              => 15
      //        case r"(?i)(sixteen|sechszehn|seize|sedici|dieciséis|zestien|dezasseis|szesnascie|saisprezece|seksten|sexton|kuusitoista) (.*)"                                                                     => 16
      //        case r"(?i)(seventeen|siebzehn|dix-sept|diciassette|diecisiete|zeventien|dezassete|siedemnascie|saptesprezece|sytten|sjutton|seitsemäntoista) (.*)"                                                 => 17
      //        case r"(?i)(eighteen|achtzehn|dix-huit|diciotto|dieciocho|achttien|dezoito|osiemnascie|optsprezece|atten|arton|kahdeksantoista) (.*)"                                                               => 18
      //        case r"(?i)(nineteen|neunzehn|dix-neuf|diciannove|diecinueve|negentien|dezanove|dziewietnascie|nouasprezece|nitten|nitton|yhdeksäntoista) (.*)"                                                     => 19
      //        case r"(?i)(twenty|zwanzig|vingt|venti|veinte|twintig|vinte|dwadziescia|douazeci|tyve|tjugo|tjue|kaksikymmentä) (.*)"                                                                               => 20
      //        case r"(?i)(twenty-one|einundzwanzig|vingtet un|ventuno|veintiuno|eenentwintig|vintee um|dwadziesciajeden|douazecisiunu|enogtyve|tjugoen|tjueen|kaksikymmentäyksi) (.*)"                            => 21
      //        case r"(?i)(twenty-two|zweiundzwanzig|vingt-deux|ventidue|veintidós|tweeëntwintig|vintee dois|dwadziesciadwa|douazecisidoi|toogtyve|tjugotvå|tjueto|kaksikymmentäkaksi) (.*)"                       => 22
      //        case r"(?i)(twenty-three|dreiundzwanzig|vingt-trois|ventitre|veintitrés|drieëntwintig|vintee três|dwadziesciatrzy|douazecisitrei|treogtyve|tjugotre|tjuetre|kaksikymmentäkolme) (.*)"               => 23
      //        case r"(?i)(twenty-four|vierundzwanzig|vingt-quatre|ventiquattro|veinticuatro|vierentwintig|vintee quatro|dwadziesciacztery|douazecisipatru|fireogtyve|tjugofyra|tjuefire|kaksikymmentäneljä) (.*)" => 24
      //
      //      }



    }
  }
}

/*
(one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|twenty-one|twenty-two|twenty-three|twenty-four|ein|zwei|drei|vier|fünf|sechs|sieben|acht|neun|zehn|elf|zwölf|dreizehn|vierzehn|funfzehn|sechszehn|siebzehn|achtzehn|neunzehn|zwanzig|einundzwanzig|zweiundzwanzig|dreiundzwanzig|vierundzwanzig|un|deux|trois|quatre|cinq|sept|huit|neuf|dix|onze|douze|treize|quatorze|quinze|seize|dix-sept|dix-huit|dix-neuf|vingt|vingtet un|vingt-deux|vingt-trois|vingt-quatre|uno|due|tre|quattro|cinque|sei|sette|otto|nove|dieci|undici|dodici|tredici|quattordici|quindici|sedici|diciassette|diciotto|diciannove|venti|ventuno|ventidue|ventitre|ventiquattro|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez|once|doce|trece|catorce|quince|dieciséis|diecisiete|dieciocho|diecinueve|veinte|veintiuno|veintidós|veintitrés|veinticuatro|een|twee|drie|vijf|zes|zeven|negen|tien|twaalf|dertien|veertien|vijftien|zestien|zeventien|achttien|negentien|twintig|eenentwintig|tweeëntwintig|drieëntwintig|vierentwintig|um|dois|três|quatro|sete|oito|dez|doze|treze|catorze|dezasseis|dezassete|dezoito|dezanove|vinte|vintee um|vintee dois|vintee três|vintee quatro|jeden|dwa|trzy|cztery|piec|szesc|siedem|osiem|dziewiec|dziesiec|jedenascie|dwanascie|trzynascie|czternascie|pietnascie|szesnascie|siedemnascie|osiemnascie|dziewietnascie|dwadziescia|dwadziesciajeden|dwadziesciadwa|dwadziesciatrzy|dwadziesciacztery|unu|doi|trei|patru|cinci|sase|sapte|opt|noua|zece|unsprezece|doisprezece|treisprezece|paisprezece|cincisprezece|saisprezece|saptesprezece|optsprezece|nouasprezece|douazeci|douazecisiunu|douazecisidoi|douazecisitrei|douazecisipatru|en|et|to|fire|fem|seks|syv|otte|ni|ti|elleve|tolv|tretten|fjorten|femten|seksten|sytten|atten|nitten|tyve|enogtyve|toogtyve|treogtyve|fireogtyve|en|ett|två|fyra|sex|sju|otta|nio|tio|elva|tretton|fjorton|femton|sexton|sjutton|arton|nitton|tjugo|tjugoen|tjugotvå|tjugotre|tjugofyra|sju|syv|åtte|tjue|tjueen|tjueto|tjuetre|tjuefire|yksi|kaksi|kolme|neljä|viisi|kuusi|seitsemän|kahdeksan|yhdeksän|kymmenen|yksitoista|kaksitoista|kolmetoista|neljätoista|viisitoista|kuusitoista|seitsemäntoista|kahdeksantoista|yhdeksäntoista|kaksikymmentä|kaksikymmentäyksi|kaksikymmentäkaksi|kaksikymmentäkolme|kaksikymmentäneljä)

(
one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|twenty-one|twenty-two|twenty-three|twenty-four|
ein|zwei|drei|vier|fünf|sechs|sieben|acht|neun|zehn|elf|zwölf|dreizehn|vierzehn|funfzehn|sechszehn|siebzehn|achtzehn|neunzehn|zwanzig|einundzwanzig|zweiundzwanzig|dreiundzwanzig|vierundzwanzig|
un|deux|trois|quatre|cinq|six|sept|huit|neuf|dix|onze|douze|treize|quatorze|quinze|seize|dix-sept|dix-huit|dix-neuf|vingt|vingtet un|vingt-deux|vingt-trois|vingt-quatre|
uno|due|tre|quattro|cinque|sei|sette|otto|nove|dieci|undici|dodici|tredici|quattordici|quindici|sedici|diciassette|diciotto|diciannove|venti|ventuno|ventidue|ventitre|ventiquattro|
uno|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez|once|doce|trece|catorce|quince|dieciséis|diecisiete|dieciocho|diecinueve|veinte|veintiuno|veintidós|veintitrés|veinticuatro|
een|twee|drie|vier|vijf|zes|zeven|acht|negen|tien|elf|twaalf|dertien|veertien|vijftien|zestien|zeventien|achttien|negentien|twintig|eenentwintig|tweeëntwintig|drieëntwintig|vierentwintig|
um|dois|três|quatro|cinco|seis|sete|oito|nove|dez|onze|doze|treze|catorze|quinze|dezasseis|dezassete|dezoito|dezanove|vinte|vintee um|vintee dois|vintee três|vintee quatro|
jeden|dwa|trzy|cztery|piec|szesc|siedem|osiem|dziewiec|dziesiec|jedenascie|dwanascie|trzynascie|czternascie|pietnascie|szesnascie|siedemnascie|osiemnascie|dziewietnascie|dwadziescia|dwadziesciajeden|dwadziesciadwa|dwadziesciatrzy|dwadziesciacztery|
unu|doi|trei|patru|cinci|sase|sapte|opt|noua|zece|unsprezece|doisprezece|treisprezece|paisprezece|cincisprezece|saisprezece|saptesprezece|optsprezece|nouasprezece|douazeci|douazecisiunu|douazecisidoi|douazecisitrei|douazecisipatru|
en|et|to|tre|fire|fem|seks|syv|otte|ni|ti|elleve|tolv|tretten|fjorten|femten|seksten|sytten|atten|nitten|tyve|enogtyve|toogtyve|treogtyve|fireogtyve|
en|ett|två|tre|fyra|fem|sex|sju|otta|nio|tio|elva|tolv|tretton|fjorton|femton|sexton|sjutton|arton|nitton|tjugo|tjugoen|tjugotvå|tjugotre|tjugofyra|
en|ett|to|tre|fire|fem|seks|sju|syv|åtte|ni|ti|elleve|tolv|tretten|fjorten|femten|seksten|sytten|atten|nitten|tjue|tjueen|tjueto|tjuetre|tjuefire|
yksi|kaksi|kolme|neljä|viisi|kuusi|seitsemän|kahdeksan|yhdeksän|kymmenen|yksitoista|kaksitoista|kolmetoista|neljätoista|viisitoista|kuusitoista|seitsemäntoista|kahdeksantoista|yhdeksäntoista|kaksikymmentä|kaksikymmentäyksi|kaksikymmentäkaksi|kaksikymmentäkolme|kaksikymmentäneljä
)

(
two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|twenty-one|twenty-two|twenty-three|twenty-four|
zwei|drei|vier|fünf|sechs|sieben|acht|neun|zehn|elf|zwölf|dreizehn|vierzehn|funfzehn|sechszehn|siebzehn|achtzehn|neunzehn|zwanzig|einundzwanzig|zweiundzwanzig|dreiundzwanzig|vierundzwanzig|
deux|trois|quatre|cinq|six|sept|huit|neuf|dix|onze|douze|treize|quatorze|quinze|seize|dix-sept|dix-huit|dix-neuf|vingt|vingtet un|vingt-deux|vingt-trois|vingt-quatre|
due|tre|quattro|cinque|sei|sette|otto|nove|dieci|undici|dodici|tredici|quattordici|quindici|sedici|diciassette|diciotto|diciannove|venti|ventuno|ventidue|ventitre|ventiquattro|
dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez|once|doce|trece|catorce|quince|dieciséis|diecisiete|dieciocho|diecinueve|veinte|veintiuno|veintidós|veintitrés|veinticuatro|
twee|drie|vier|vijf|zes|zeven|acht|negen|tien|elf|twaalf|dertien|veertien|vijftien|zestien|zeventien|achttien|negentien|twintig|eenentwintig|tweeëntwintig|drieëntwintig|vierentwintig|
dois|três|quatro|cinco|seis|sete|oito|nove|dez|onze|doze|treze|catorze|quinze|dezasseis|dezassete|dezoito|dezanove|vinte|vintee um|vintee dois|vintee três|vintee quatro|
dwa|trzy|cztery|piec|szesc|siedem|osiem|dziewiec|dziesiec|jedenascie|dwanascie|trzynascie|czternascie|pietnascie|szesnascie|siedemnascie|osiemnascie|dziewietnascie|dwadziescia|dwadziesciajeden|dwadziesciadwa|dwadziesciatrzy|dwadziesciacztery|
doi|trei|patru|cinci|sase|sapte|opt|noua|zece|unsprezece|doisprezece|treisprezece|paisprezece|cincisprezece|saisprezece|saptesprezece|optsprezece|nouasprezece|douazeci|douazecisiunu|douazecisidoi|douazecisitrei|douazecisipatru|
to|tre|fire|fem|seks|syv|otte|ni|ti|elleve|tolv|tretten|fjorten|femten|seksten|sytten|atten|nitten|tyve|enogtyve|toogtyve|treogtyve|fireogtyve|
två|tre|fyra|fem|sex|sju|otta|nio|tio|elva|tolv|tretton|fjorton|femton|sexton|sjutton|arton|nitton|tjugo|tjugoen|tjugotvå|tjugotre|tjugofyra|
to|tre|fire|fem|seks|sju|syv|åtte|ni|ti|elleve|tolv|tretten|fjorten|femten|seksten|sytten|atten|nitten|tjue|tjueen|tjueto|tjuetre|tjuefire|
kaksi|kolme|neljä|viisi|kuusi|seitsemän|kahdeksan|yhdeksän|kymmenen|yksitoista|kaksitoista|kolmetoista|neljätoista|viisitoista|kuusitoista|seitsemäntoista|kahdeksantoista|yhdeksäntoista|kaksikymmentä|kaksikymmentäyksi|kaksikymmentäkaksi|kaksikymmentäkolme|kaksikymmentäneljä
)


case r"(?i)(one|ein|un|uno|een|um|jeden|unu|en|et|en|ett|yksi)$x (.*)$z" => 1


case r"(?i)(two|zwei|deux|due|dos|twee|dois|dwa|doi|to|två|kaksi)$x (.*)$z" => 2
case r"(?i)(three|drei|trois|tre|tres|drie|três|trzy|trei|kolme)$x (.*)$z" => 3
case r"(?i)(four|vier|quatre|quattro|cuatro|quatro|cztery|patru|fire|fyra|neljä)$x (.*)$z" => 4
case r"(?i)(five|fünf|cinq|cinque|cinco|vijf|piec|cinci|fem|viisi)$x (.*)$z" => 5
case r"(?i)(six|sechs|sei|seis|zes|szesc|sase|seks|sex|kuusi)$x (.*)$z" => 6
case r"(?i)(seven|sieben|sept|sette|siete|zeven|sete|siedem|sapte|syv|sju|sju|syv|seitsemän)$x (.*)$z" => 7
case r"(?i)(eight|acht|huit|otto|ocho|oito|osiem|opt|otte|otta|åtte|kahdeksan)$x (.*)$z" => 8
case r"(?i)(nine|neun|neuf|nove|nueve|negen|dziewiec|noua|ni|nio|yhdeksän)$x (.*)$z" => 9
case r"(?i)(ten|zehn|dix|dieci|diez|tien|dez|dziesiec|zece|ti|tio|kymmenen)$x (.*)$z" => 10
case r"(?i)(eleven|elf|onze|undici|once|jedenascie|unsprezece|elleve|elva|yksitoista)$x (.*)$z" => 11
case r"(?i)(twelve|zwölf|douze|dodici|doce|twaalf|doze|dwanascie|doisprezece|tolv|kaksitoista)$x (.*)$z" => 12
case r"(?i)(thirteen|dreizehn|treize|tredici|trece|dertien|treze|trzynascie|treisprezece|tretten|tretton|kolmetoista)$x (.*)$z" => 13
case r"(?i)(fourteen|vierzehn|quatorze|quattordici|catorce|veertien|catorze|czternascie|paisprezece|fjorten|fjorton|neljätoista)$x (.*)$z" => 14
case r"(?i)(fifteen|funfzehn|quinze|quindici|quince|vijftien|pietnascie|cincisprezece|femten|femton|viisitoista)$x (.*)$z" => 15
case r"(?i)(sixteen|sechszehn|seize|sedici|dieciséis|zestien|dezasseis|szesnascie|saisprezece|seksten|sexton|kuusitoista)$x (.*)$z" => 16
case r"(?i)(seventeen|siebzehn|dix-sept|diciassette|diecisiete|zeventien|dezassete|siedemnascie|saptesprezece|sytten|sjutton|seitsemäntoista)$x (.*)$z" => 17
case r"(?i)(eighteen|achtzehn|dix-huit|diciotto|dieciocho|achttien|dezoito|osiemnascie|optsprezece|atten|arton|kahdeksantoista)$x (.*)$z" => 18
case r"(?i)(nineteen|neunzehn|dix-neuf|diciannove|diecinueve|negentien|dezanove|dziewietnascie|nouasprezece|nitten|nitton|yhdeksäntoista)$x (.*)$z" => 19
case r"(?i)(twenty|zwanzig|vingt|venti|veinte|twintig|vinte|dwadziescia|douazeci|tyve|tjugo|tjue|kaksikymmentä)$x (.*)$z" => 20
case r"(?i)(twenty-one|einundzwanzig|vingtet un|ventuno|veintiuno|eenentwintig|vintee um|dwadziesciajeden|douazecisiunu|enogtyve|tjugoen|tjueen|kaksikymmentäyksi)$x (.*)$z" => 21
case r"(?i)(twenty-two|zweiundzwanzig|vingt-deux|ventidue|veintidós|tweeëntwintig|vintee dois|dwadziesciadwa|douazecisidoi|toogtyve|tjugotvå|tjueto|kaksikymmentäkaksi)$x (.*)$z" => 22
case r"(?i)(twenty-three|dreiundzwanzig|vingt-trois|ventitre|veintitrés|drieëntwintig|vintee três|dwadziesciatrzy|douazecisitrei|treogtyve|tjugotre|tjuetre|kaksikymmentäkolme)$x (.*)$z" => 23
case r"(?i)(twenty-four|vierundzwanzig|vingt-quatre|ventiquattro|veinticuatro|vierentwintig|vintee quatro|dwadziesciacztery|douazecisipatru|fireogtyve|tjugofyra|tjuefire|kaksikymmentäneljä)$x (.*)$z" => 24


case r"(?i)(one|ein|un|uno|een|um|jeden|unu|en|et|en|ett|yksi)$x (.*)$s" => s
case r"(?i)(two|zwei|deux|due|dos|twee|dois|dwa|doi|to|två|kaksi)$x (.*)$s" => s
case r"(?i)(three|drei|trois|tre|tres|drie|três|trzy|trei|kolme)$x (.*)$s" => s
case r"(?i)(four|vier|quatre|quattro|cuatro|quatro|cztery|patru|fire|fyra|neljä)$x (.*)$s" => s
case r"(?i)(five|fünf|cinq|cinque|cinco|vijf|piec|cinci|fem|viisi)$x (.*)$s" => s
case r"(?i)(six|sechs|sei|seis|zes|szesc|sase|seks|sex|kuusi)$x (.*)$s" => s
case r"(?i)(seven|sieben|sept|sette|siete|zeven|sete|siedem|sapte|syv|sju|sju|syv|seitsemän)$x (.*)$s" => s
case r"(?i)(eight|acht|huit|otto|ocho|oito|osiem|opt|otte|otta|åtte|kahdeksan)$x (.*)$s" => s
case r"(?i)(nine|neun|neuf|nove|nueve|negen|dziewiec|noua|ni|nio|yhdeksän)$x (.*)$s" => s
case r"(?i)(ten|zehn|dix|dieci|diez|tien|dez|dziesiec|zece|ti|tio|kymmenen)$x (.*)$s" => s
case r"(?i)(eleven|elf|onze|undici|once|jedenascie|unsprezece|elleve|elva|yksitoista)$x (.*)$s" => s
case r"(?i)(twelve|zwölf|douze|dodici|doce|twaalf|doze|dwanascie|doisprezece|tolv|kaksitoista)$x (.*)$s" => s
case r"(?i)(thirteen|dreizehn|treize|tredici|trece|dertien|treze|trzynascie|treisprezece|tretten|tretton|kolmetoista)$x (.*)$s" => s
case r"(?i)(fourteen|vierzehn|quatorze|quattordici|catorce|veertien|catorze|czternascie|paisprezece|fjorten|fjorton|neljätoista)$x (.*)$s" => s
case r"(?i)(fifteen|funfzehn|quinze|quindici|quince|vijftien|pietnascie|cincisprezece|femten|femton|viisitoista)$x (.*)$s" => s
case r"(?i)(sixteen|sechszehn|seize|sedici|dieciséis|zestien|dezasseis|szesnascie|saisprezece|seksten|sexton|kuusitoista)$x (.*)$s" => s
case r"(?i)(seventeen|siebzehn|dix-sept|diciassette|diecisiete|zeventien|dezassete|siedemnascie|saptesprezece|sytten|sjutton|seitsemäntoista)$x (.*)$s" => s
case r"(?i)(eighteen|achtzehn|dix-huit|diciotto|dieciocho|achttien|dezoito|osiemnascie|optsprezece|atten|arton|kahdeksantoista)$x (.*)$s" => s
case r"(?i)(nineteen|neunzehn|dix-neuf|diciannove|diecinueve|negentien|dezanove|dziewietnascie|nouasprezece|nitten|nitton|yhdeksäntoista)$x (.*)$s" => s
case r"(?i)(twenty|zwanzig|vingt|venti|veinte|twintig|vinte|dwadziescia|douazeci|tyve|tjugo|tjue|kaksikymmentä)$x (.*)$s" => s
case r"(?i)(twenty-one|einundzwanzig|vingtet un|ventuno|veintiuno|eenentwintig|vintee um|dwadziesciajeden|douazecisiunu|enogtyve|tjugoen|tjueen|kaksikymmentäyksi)$x (.*)$s" => s
case r"(?i)(twenty-two|zweiundzwanzig|vingt-deux|ventidue|veintidós|tweeëntwintig|vintee dois|dwadziesciadwa|douazecisidoi|toogtyve|tjugotvå|tjueto|kaksikymmentäkaksi)$x (.*)$s" => s
case r"(?i)(twenty-three|dreiundzwanzig|vingt-trois|ventitre|veintitrés|drieëntwintig|vintee três|dwadziesciatrzy|douazecisitrei|treogtyve|tjugotre|tjuetre|kaksikymmentäkolme)$x (.*)$s" => s
case r"(?i)(twenty-four|vierundzwanzig|vingt-quatre|ventiquattro|veinticuatro|vierentwintig|vintee quatro|dwadziesciacztery|douazecisipatru|fireogtyve|tjugofyra|tjuefire|kaksikymmentäneljä)$x (.*)$s" => s


*/