package moleculeadmin.servertest

import java.util.regex.Pattern
import db.core.dsl.coreTest._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.server.utils.DateStrLocal
import moleculeadmin.servertest.ResetDbs.protocol
import moleculeadmin.shared.ast.schema.MetaSchema
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import moleculeadmin.shared.testdata.{CoreSchema, ExampleData}
import moleculeadmin.shared.util.HelpersAdmin
import utest._
import scala.languageFeature.implicitConversions._


object Adhoc extends TestSuite
  with HelpersAdmin
  with ExampleData
  with CoreSchema
  with DateStrLocal
  with ModelOps
  with ColOps {

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("Adhoc") {

      //      implicit val conn = recreateDbFrom(CoreTestSchema, "localhost:4334/CoreTest", protocol)
      //      implicit val conn = Conn(base + "/MoleculeAdmin")
      //      implicit val conn = Conn(base + "/mbrainz-1968-1973")
      //      implicit val conn = Conn(base + "/Partition")
      //      implicit val conn = Conn(base + "/Partition1")
      //      implicit val conn = Conn(base + "/Partition2")
      //      recreateDbFrom(PartitionSchema, "localhost:4334/Partition", protocol)
      //      implicit val conn = Conn(base + "/Clazzig")


      //      implicit val conn = Conn(base + "/CoreTest")
      //      implicit val conn = recreateDbFrom(CoreTestSchema)

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


      val numbersTransposed = numbers.transpose

      val cases = numbersTransposed.distinct.zipWithIndex.map { case (regexes, i) =>
        regexes.mkString("case \"", "\" | \"", "\" => " + (i + 1))
      }

      cases foreach println



      //      Ns.e.ref1$.int(1)

    }
  }
}