package db.integration.schema

import molecule.schema.definition._

@InOut(0, 8)
object MBrainzDefinition {

  trait AbstractRelease {
    val name         = oneString
    val artistCredit = oneString.fulltext
    val gid          = oneUUID.uniqueIdentity
    val `type`       = oneEnum("album", "single", "ep", "audiobook", "other")
    val artists      = many[Artist]
  }

  trait Artist {
    val startYear  = oneLong
    val startMonth = oneLong
    val startDay   = oneLong
    val endYear    = oneLong
    val endMonth   = oneLong
    val endDay     = oneLong
    val sortName   = oneString
    val name       = oneString.fulltext
    val gid        = oneUUID.uniqueIdentity
    val country    = one[Country]
    val `type`     = oneEnum("person", "group", "other")
    val gender     = oneEnum("male", "female", "other")
  }

  trait Country {
    val name = oneString.uniqueValue
  }

  trait Label {
    val startYear  = oneLong
    val startMonth = oneLong
    val startDay   = oneLong
    val endYear    = oneLong
    val endMonth   = oneLong
    val endDay     = oneLong
    val sortName   = oneString
    val name       = oneString.fulltext
    val gid        = oneUUID.uniqueIdentity
    val country    = one[Country]
    val `type`     = oneEnum("distributor", "holding", "production", "originalProduction", "bootlegProduction", "reissueProduction", "publisher")
  }

  trait Language {
    val name = oneString.uniqueValue
  }

  trait Medium {
    val name       = oneString.fulltext
    val position   = oneLong
    val trackCount = oneLong
    val format     = oneEnum("dvdVideo", "laserDisc", "cd", "hddvd", "vhs", "svcd", "dcc", "cdr", "slotMusic", "bluray", "waxCylinder", "cartridge", "umd", "miniDisc", "vinyl", "vinyl12", "sacd", "other", "dualDisc", "vinyl10", "dvd", "pianoRoll", "betamax", "vcd", "dat", "reel", "vinyl7", "dvdAudio", "digitalMedia", "hdcd", "videotape", "usbFlashDrive", "cassette", "cd8cm")
    val tracks     = many[Track].isComponent
  }

  trait Release {
    val year            = oneLong
    val month           = oneLong
    val day             = oneLong
    val artistCredit    = oneString.fulltext
    val status          = oneString
    val barcode         = oneString
    val name            = oneString.fulltext
    val gid             = oneUUID.uniqueIdentity
    val artists         = many[Artist]
    val abstractRelease = one[AbstractRelease]
    val language        = one[Language]
    val media           = many[Medium].isComponent
    val packaging       = oneEnum("jewelCase", "slimJewelCase", "digipak", "none", "keepCase", "cardboardPaperSleeve", "other")
    val script          = one[Script]
    // wrongly listed as card-one `label` on https://github.com/Datomic/mbrainz-sample/wiki/Schema
    val labels          = many[Label]
    val country         = one[Country]
  }

  trait Script {
    val name = oneString.uniqueValue
  }

  trait Track {
    val position     = oneLong
    val duration     = oneLong
    val artistCredit = oneString.fulltext
    val name         = oneString.fulltext
    val artists      = many[Artist]
  }

  //  trait Artist {
  //    val startYear  = oneLong
  //    val startMonth = oneLong
  //    val startDay   = oneLong
  //    val endYear    = oneLong
  //    val endMonth   = oneLong
  //    val endDay     = oneLong
  //    val sortName   = oneString
  //    val name       = oneString.fulltext
  //    val gid        = oneUUID.uniqueIdentity
  //    val `type`     = oneEnum("person", "group", "other")
  //    val gender     = oneEnum("male", "female", "other")
  //    val country    = one[Country]
  //  }
  //
  //  trait Country {
  //    val name = oneString.uniqueValue
  //  }
  //
  //  trait Medium {
  //    val position   = oneLong
  //    val trackCount = oneLong
  //    val format     = oneEnum("dvdVideo", "laserDisc", "cd", "hddvd", "vhs", "svcd", "dcc", "cdr", "slotMusic", "bluray", "waxCylinder", "cartridge", "umd", "miniDisc", "vinyl", "vinyl12", "sacd", "other", "dualDisc", "vinyl10", "dvd", "pianoRoll", "betamax", "vcd", "dat", "reel", "vinyl7", "dvdAudio", "digitalMedia", "hdcd", "videotape", "usbFlashDrive", "cassette", "cd8cm")
  //    val tracks     = many[Track].isComponent
  //  }
  //
  //  trait Release {
  //    val year            = oneLong
  //    val month           = oneLong
  //    val day             = oneLong
  //    val artistCredit    = oneString.fulltext
  //    val status          = oneString
  //    val barcode         = oneString
  //    val name            = oneString.fulltext
  //    val gid             = oneUUID.uniqueIdentity
  //    val artists         = many[Artist]
  //    val media           = many[Medium].isComponent
  //    val packaging       = oneEnum("jewelCase", "slimJewelCase", "digipak", "none", "keepCase", "cardboardPaperSleeve", "other")
  //    val country         = one[Country]
  //  }
  //
  //  trait Track {
  //    val position = oneLong
  //    val duration = oneLong
  //    val name     = oneString.fulltext
  //    val artists  = many[Artist]
  //  }
}