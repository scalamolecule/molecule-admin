package moleculeadmin.shared.testdata

import moleculeadmin.shared.ast.schema._

trait mBrainzSchema {

  implicit val nsMap: Map[String, Ns] = Map(
    "AbstractRelease" -> Ns(1, "AbstractRelease", "AbstractRelease", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "name", 1, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(2, "artistCredit", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(3, "gid", 1, "UUID", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, Seq()),
      Attr(4, "type", 1, "String", Some(Set("other", "single", "album", "ep", "audiobook")), None, None, None, None, None, None, None, Seq()),
      Attr(5, "artists", 2, "ref", None, Some("Artist"), None, None, None, None, None, Some("name"), Seq()))),

    "Country" -> Ns(3, "Country", "Country", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "name", 1, "String", None, None, Some(Set("uniqueValue")), None, None, None, None, None, Seq()))),

    "Release" -> Ns(7, "Release", "Release", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "year", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(2, "month", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(3, "day", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(4, "artistCredit", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(5, "status", 1, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(6, "barcode", 1, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(7, "name", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(8, "gid", 1, "UUID", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, Seq()),
      Attr(9, "artists", 2, "ref", None, Some("Artist"), None, None, None, None, None, Some("name"), Seq()),
      Attr(10, "abstractRelease", 1, "ref", None, Some("AbstractRelease"), None, None, None, None, None, Some("name"), Seq()),
      Attr(11, "language", 1, "ref", None, Some("Language"), None, None, None, None, None, Some("name"), Seq()),
      Attr(12, "media", 2, "ref", None, Some("Medium"), Some(Set("isComponent")), None, None, None, None, Some("name"), Seq()),
      Attr(13, "packaging", 1, "String", Some(Set("other", "keepCase", "slimJewelCase", "digipak", "none", "cardboardPaperSleeve", "jewelCase")), None, None, None, None, None, None, None, Seq()),
      Attr(14, "script", 1, "ref", None, Some("Script"), None, None, None, None, None, Some("name"), Seq()),
      Attr(15, "labels", 2, "ref", None, Some("Label"), None, None, Some("wrongly listed as card-one `label` on https://github.com/Datomic/mbrainz-sample/wiki/Schema"), None, None, Some("name"), Seq()),
      Attr(16, "country", 1, "ref", None, Some("Country"), None, None, None, None, None, Some("name"), Seq()))),

    "Medium" -> Ns(6, "Medium", "Medium", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "name", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(2, "position", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(3, "trackCount", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(4, "format", 1, "String", Some(Set("other", "dvdVideo", "betamax", "cdr", "slotMusic", "dcc", "vinyl12", "pianoRoll", "usbFlashDrive", "dvdAudio", "cassette", "waxCylinder", "dvd", "vinyl7", "cd", "vinyl", "hddvd", "sacd", "vinyl10", "videotape", "miniDisc", "dualDisc", "vcd", "cd8cm", "vhs", "reel", "dat", "hdcd", "digitalMedia", "umd", "laserDisc", "cartridge", "svcd", "bluray")), None, None, None, None, None, None, None, Seq()),
      Attr(5, "tracks", 2, "ref", None, Some("Track"), Some(Set("isComponent")), None, None, None, None, Some("name"), Seq()))),

    "Track" -> Ns(9, "Track", "Track", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "position", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(2, "duration", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(3, "artistCredit", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(4, "name", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(5, "artists", 2, "ref", None, Some("Artist"), None, None, None, None, None, Some("name"), Seq()))),

    "Language" -> Ns(5, "Language", "Language", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "name", 1, "String", None, None, Some(Set("uniqueValue")), None, None, None, None, None, Seq()))),

    "Script" -> Ns(8, "Script", "Script", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "name", 1, "String", None, None, Some(Set("uniqueValue")), None, None, None, None, None, Seq()))),

    "Label" -> Ns(4, "Label", "Label", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "startYear", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(2, "startMonth", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(3, "startDay", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(4, "endYear", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(5, "endMonth", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(6, "endDay", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(7, "sortName", 1, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(8, "name", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(9, "gid", 1, "UUID", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, Seq()),
      Attr(10, "country", 1, "ref", None, Some("Country"), None, None, None, None, None, Some("name"), Seq()),
      Attr(11, "type", 1, "String", Some(Set("publisher", "production", "reissueProduction", "distributor", "originalProduction", "bootlegProduction", "holding")), None, None, None, None, None, None, None, Seq()))),

    "Artist" -> Ns(2, "Artist", "Artist", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "startYear", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(2, "startMonth", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(3, "startDay", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(4, "endYear", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(5, "endMonth", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(6, "endDay", 1, "Long", None, None, None, None, None, None, None, None, Seq()),
      Attr(7, "sortName", 1, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(8, "name", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(9, "gid", 1, "UUID", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, Seq()),
      Attr(10, "country", 1, "ref", None, Some("Country"), None, None, None, None, None, Some("name"), Seq()),
      Attr(11, "type", 1, "String", Some(Set("group", "person", "other")), None, None, None, None, None, None, None, Seq()),
      Attr(12, "gender", 1, "String", Some(Set("male", "female", "other")), None, None, None, None, None, None, None, Seq()))),
  )
}

object mBrainzSchema extends mBrainzSchema
