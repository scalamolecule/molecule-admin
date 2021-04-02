package moleculeadmin.shared.testdata

import moleculeadmin.shared.ast.metaSchema._

trait mBrainzSchema {

  implicit val nsMap: Map[String, MetaNs] = Map(
    "AbstractRelease" -> MetaNs(1, "AbstractRelease", "AbstractRelease", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "name", 1, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(2, "artistCredit", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(3, "gid", 1, "UUID", Nil, None, Seq("uniqueIdentity"), None, None, None, None, None, Seq()),
      MetaAttr(4, "type", 1, "String", Seq("other", "single", "album", "ep", "audiobook"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(5, "artists", 2, "ref", Nil, Some("Artist"), Nil, None, None, None, None, Some("name"), Seq()))),

    "Country" -> MetaNs(3, "Country", "Country", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "name", 1, "String", Nil, None, Seq("uniqueValue"), None, None, None, None, None, Seq()))),

    "Release" -> MetaNs(7, "Release", "Release", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "year", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(2, "month", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(3, "day", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(4, "artistCredit", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(5, "status", 1, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(6, "barcode", 1, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(7, "name", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(8, "gid", 1, "UUID", Nil, None, Seq("uniqueIdentity"), None, None, None, None, None, Seq()),
      MetaAttr(9, "artists", 2, "ref", Nil, Some("Artist"), Nil, None, None, None, None, Some("name"), Seq()),
      MetaAttr(10, "abstractRelease", 1, "ref", Nil, Some("AbstractRelease"), Nil, None, None, None, None, Some("name"), Seq()),
      MetaAttr(11, "language", 1, "ref", Nil, Some("Language"), Nil, None, None, None, None, Some("name"), Seq()),
      MetaAttr(12, "media", 2, "ref", Nil, Some("Medium"), Seq("isComponent"), None, None, None, None, Some("name"), Seq()),
      MetaAttr(13, "packaging", 1, "String", Seq("other", "keepCase", "slimJewelCase", "digipak", "none", "cardboardPaperSleeve", "jewelCase"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(14, "script", 1, "ref", Nil, Some("Script"), Nil, None, None, None, None, Some("name"), Seq()),
      MetaAttr(15, "labels", 2, "ref", Nil, Some("Label"), Nil, None, Some("wrongly listed as card-one `label` on https://github.com/Datomic/mbrainz-sample/wiki/Schema"), None, None, Some("name"), Seq()),
      MetaAttr(16, "country", 1, "ref", Nil, Some("Country"), Nil, None, None, None, None, Some("name"), Seq()))),

    "Medium" -> MetaNs(6, "Medium", "Medium", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "name", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(2, "position", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(3, "trackCount", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(4, "format", 1, "String", Seq("other", "dvdVideo", "betamax", "cdr", "slotMusic", "dcc", "vinyl12", "pianoRoll", "usbFlashDrive", "dvdAudio", "cassette", "waxCylinder", "dvd", "vinyl7", "cd", "vinyl", "hddvd", "sacd", "vinyl10", "videotape", "miniDisc", "dualDisc", "vcd", "cd8cm", "vhs", "reel", "dat", "hdcd", "digitalMedia", "umd", "laserDisc", "cartridge", "svcd", "bluray"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(5, "tracks", 2, "ref", Nil, Some("Track"), Seq("isComponent"), None, None, None, None, Some("name"), Seq()))),

    "Track" -> MetaNs(9, "Track", "Track", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "position", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(2, "duration", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(3, "artistCredit", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(4, "name", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(5, "artists", 2, "ref", Nil, Some("Artist"), Nil, None, None, None, None, Some("name"), Seq()))),

    "Language" -> MetaNs(5, "Language", "Language", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "name", 1, "String", Nil, None, Seq("uniqueValue"), None, None, None, None, None, Seq()))),

    "Script" -> MetaNs(8, "Script", "Script", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "name", 1, "String", Nil, None, Seq("uniqueValue"), None, None, None, None, None, Seq()))),

    "Label" -> MetaNs(4, "Label", "Label", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "startYear", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(2, "startMonth", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(3, "startDay", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(4, "endYear", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(5, "endMonth", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(6, "endDay", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(7, "sortName", 1, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(8, "name", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(9, "gid", 1, "UUID", Nil, None, Seq("uniqueIdentity"), None, None, None, None, None, Seq()),
      MetaAttr(10, "country", 1, "ref", Nil, Some("Country"), Nil, None, None, None, None, Some("name"), Seq()),
      MetaAttr(11, "type", 1, "String", Seq("publisher", "production", "reissueProduction", "distributor", "originalProduction", "bootlegProduction", "holding"), None, Nil, None, None, None, None, None, Seq()))),

    "Artist" -> MetaNs(2, "Artist", "Artist", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "startYear", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(2, "startMonth", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(3, "startDay", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(4, "endYear", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(5, "endMonth", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(6, "endDay", 1, "Long", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(7, "sortName", 1, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(8, "name", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(9, "gid", 1, "UUID", Nil, None, Seq("uniqueIdentity"), None, None, None, None, None, Seq()),
      MetaAttr(10, "country", 1, "ref", Nil, Some("Country"), Nil, None, None, None, None, Some("name"), Seq()),
      MetaAttr(11, "type", 1, "String", Seq("group", "person", "other"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(12, "gender", 1, "String", Seq("male", "female", "other"), None, Nil, None, None, None, None, None, Seq()))),
  )
}

object mBrainzSchema extends mBrainzSchema
