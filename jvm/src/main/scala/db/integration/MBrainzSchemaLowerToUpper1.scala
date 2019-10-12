/*
* AUTO-GENERATED Molecule DSL schema boilerplate code
*
* To change:
* 1. edit schema definition file in `db.integration.schema/`
* 2. `sbt compile` in terminal
* 3. Refresh and re-compile project in IDE
*/
package db.integration

import datomic.Util

object MBrainzSchemaLowerToUpper1 {

  lazy val namespaces = Util.list(

    // AbstractRelease --------------------------------------------------

    Util.map(":db/id", ":abstractRelease/name", ":db/ident", ":AbstractRelease/name"),

    Util.map(":db/id", ":abstractRelease/artistCredit", ":db/ident", ":AbstractRelease/artistCredit"),

    Util.map(":db/id", ":abstractRelease/gid", ":db/ident", ":AbstractRelease/gid"),

    Util.map(":db/id", ":abstractRelease/type", ":db/ident", ":AbstractRelease/type"),

    Util.map(":db/id", ":release.type/album", ":db/ident", ":AbstractRelease.type/album"),
    Util.map(":db/id", ":release.type/single", ":db/ident", ":AbstractRelease.type/single"),
    Util.map(":db/id", ":release.type/ep", ":db/ident", ":AbstractRelease.type/ep"),
    Util.map(":db/id", ":release.type/audiobook", ":db/ident", ":AbstractRelease.type/audiobook"),
    Util.map(":db/id", ":release.type/other", ":db/ident", ":AbstractRelease.type/other"),

    Util.map(":db/id", ":abstractRelease/artists", ":db/ident", ":AbstractRelease/artists"),


    // Artist -----------------------------------------------------------

    Util.map(":db/id", ":artist/startYear", ":db/ident", ":Artist/startYear"),

    Util.map(":db/id", ":artist/startMonth", ":db/ident", ":Artist/startMonth"),

    Util.map(":db/id", ":artist/startDay", ":db/ident", ":Artist/startDay"),

    Util.map(":db/id", ":artist/endYear", ":db/ident", ":Artist/endYear"),

    Util.map(":db/id", ":artist/endMonth", ":db/ident", ":Artist/endMonth"),

    Util.map(":db/id", ":artist/endDay", ":db/ident", ":Artist/endDay"),

    Util.map(":db/id", ":artist/sortName", ":db/ident", ":Artist/sortName"),

    Util.map(":db/id", ":artist/name", ":db/ident", ":Artist/name"),

    Util.map(":db/id", ":artist/gid", ":db/ident", ":Artist/gid"),

    Util.map(":db/id", ":artist/country", ":db/ident", ":Artist/country"),

    Util.map(":db/id", ":artist/type", ":db/ident", ":Artist/type"),

    Util.map(":db/id", ":artist.type/person", ":db/ident", ":Artist.type/person"),
    Util.map(":db/id", ":artist.type/group", ":db/ident", ":Artist.type/group"),
    Util.map(":db/id", ":artist.type/other", ":db/ident", ":Artist.type/other"),

    Util.map(":db/id", ":artist/gender", ":db/ident", ":Artist/gender"),

    Util.map(":db/id", ":artist.gender/male", ":db/ident", ":Artist.gender/male"),
    Util.map(":db/id", ":artist.gender/female", ":db/ident", ":Artist.gender/female"),
    Util.map(":db/id", ":artist.gender/other", ":db/ident", ":Artist.gender/other"),


    // Country ----------------------------------------------------------

    Util.map(":db/id", ":country/name", ":db/ident", ":Country/name"),


    // Label ------------------------------------------------------------

    Util.map(":db/id", ":label/startYear", ":db/ident", ":Label/startYear"),

    Util.map(":db/id", ":label/startMonth", ":db/ident", ":Label/startMonth"),

    Util.map(":db/id", ":label/startDay", ":db/ident", ":Label/startDay"),

    Util.map(":db/id", ":label/endYear", ":db/ident", ":Label/endYear"),

    Util.map(":db/id", ":label/endMonth", ":db/ident", ":Label/endMonth"),

    Util.map(":db/id", ":label/endDay", ":db/ident", ":Label/endDay"),

    Util.map(":db/id", ":label/sortName", ":db/ident", ":Label/sortName"),

    Util.map(":db/id", ":label/name", ":db/ident", ":Label/name"),

    Util.map(":db/id", ":label/gid", ":db/ident", ":Label/gid"),

    Util.map(":db/id", ":label/country", ":db/ident", ":Label/country"),

    Util.map(":db/id", ":label/type", ":db/ident", ":Label/type"),

    Util.map(":db/id", ":label.type/distributor", ":db/ident", ":Label.type/distributor"),
    Util.map(":db/id", ":label.type/holding", ":db/ident", ":Label.type/holding"),
    Util.map(":db/id", ":label.type/production", ":db/ident", ":Label.type/production"),
    Util.map(":db/id", ":label.type/originalProduction", ":db/ident", ":Label.type/originalProduction"),
    Util.map(":db/id", ":label.type/bootlegProduction", ":db/ident", ":Label.type/bootlegProduction"),
    Util.map(":db/id", ":label.type/reissueProduction", ":db/ident", ":Label.type/reissueProduction"),
    Util.map(":db/id", ":label.type/publisher", ":db/ident", ":Label.type/publisher"),


    // Language ---------------------------------------------------------

    Util.map(":db/id", ":language/name", ":db/ident", ":Language/name"),


    // Medium -----------------------------------------------------------

    Util.map(":db/id", ":medium/name", ":db/ident", ":Medium/name"),

    Util.map(":db/id", ":medium/position", ":db/ident", ":Medium/position"),

    Util.map(":db/id", ":medium/trackCount", ":db/ident", ":Medium/trackCount"),

    Util.map(":db/id", ":medium/format", ":db/ident", ":Medium/format"),

    Util.map(":db/id", ":medium.format/dvdVideo", ":db/ident", ":Medium.format/dvdVideo"),
    Util.map(":db/id", ":medium.format/laserDisc", ":db/ident", ":Medium.format/laserDisc"),
    Util.map(":db/id", ":medium.format/cd", ":db/ident", ":Medium.format/cd"),
    Util.map(":db/id", ":medium.format/hddvd", ":db/ident", ":Medium.format/hddvd"),
    Util.map(":db/id", ":medium.format/vhs", ":db/ident", ":Medium.format/vhs"),
    Util.map(":db/id", ":medium.format/svcd", ":db/ident", ":Medium.format/svcd"),
    Util.map(":db/id", ":medium.format/dcc", ":db/ident", ":Medium.format/dcc"),
    Util.map(":db/id", ":medium.format/cdr", ":db/ident", ":Medium.format/cdr"),
    Util.map(":db/id", ":medium.format/slotMusic", ":db/ident", ":Medium.format/slotMusic"),
    Util.map(":db/id", ":medium.format/bluray", ":db/ident", ":Medium.format/bluray"),
    Util.map(":db/id", ":medium.format/waxCylinder", ":db/ident", ":Medium.format/waxCylinder"),
    Util.map(":db/id", ":medium.format/cartridge", ":db/ident", ":Medium.format/cartridge"),
    Util.map(":db/id", ":medium.format/umd", ":db/ident", ":Medium.format/umd"),
    Util.map(":db/id", ":medium.format/miniDisc", ":db/ident", ":Medium.format/miniDisc"),
    Util.map(":db/id", ":medium.format/vinyl", ":db/ident", ":Medium.format/vinyl"),
    Util.map(":db/id", ":medium.format/vinyl12", ":db/ident", ":Medium.format/vinyl12"),
    Util.map(":db/id", ":medium.format/sacd", ":db/ident", ":Medium.format/sacd"),
    Util.map(":db/id", ":medium.format/other", ":db/ident", ":Medium.format/other"),
    Util.map(":db/id", ":medium.format/dualDisc", ":db/ident", ":Medium.format/dualDisc"),
    Util.map(":db/id", ":medium.format/vinyl10", ":db/ident", ":Medium.format/vinyl10"),
    Util.map(":db/id", ":medium.format/dvd", ":db/ident", ":Medium.format/dvd"),
    Util.map(":db/id", ":medium.format/pianoRoll", ":db/ident", ":Medium.format/pianoRoll"),
    Util.map(":db/id", ":medium.format/betamax", ":db/ident", ":Medium.format/betamax"),
    Util.map(":db/id", ":medium.format/vcd", ":db/ident", ":Medium.format/vcd"),
    Util.map(":db/id", ":medium.format/dat", ":db/ident", ":Medium.format/dat"),
    Util.map(":db/id", ":medium.format/reel", ":db/ident", ":Medium.format/reel"),
    Util.map(":db/id", ":medium.format/vinyl7", ":db/ident", ":Medium.format/vinyl7"),
    Util.map(":db/id", ":medium.format/dvdAudio", ":db/ident", ":Medium.format/dvdAudio"),
    Util.map(":db/id", ":medium.format/digitalMedia", ":db/ident", ":Medium.format/digitalMedia"),
    Util.map(":db/id", ":medium.format/hdcd", ":db/ident", ":Medium.format/hdcd"),
    Util.map(":db/id", ":medium.format/videotape", ":db/ident", ":Medium.format/videotape"),
    Util.map(":db/id", ":medium.format/usbFlashDrive", ":db/ident", ":Medium.format/usbFlashDrive"),
    Util.map(":db/id", ":medium.format/cassette", ":db/ident", ":Medium.format/cassette"),
    Util.map(":db/id", ":medium.format/cd8cm", ":db/ident", ":Medium.format/cd8cm"),

    Util.map(":db/id", ":medium/tracks", ":db/ident", ":Medium/tracks"),


    // Release ----------------------------------------------------------

    Util.map(":db/id", ":release/year", ":db/ident", ":Release/year"),

    Util.map(":db/id", ":release/month", ":db/ident", ":Release/month"),

    Util.map(":db/id", ":release/day", ":db/ident", ":Release/day"),

    Util.map(":db/id", ":release/artistCredit", ":db/ident", ":Release/artistCredit"),

    Util.map(":db/id", ":release/status", ":db/ident", ":Release/status"),

    Util.map(":db/id", ":release/barcode", ":db/ident", ":Release/barcode"),

    Util.map(":db/id", ":release/name", ":db/ident", ":Release/name"),

    Util.map(":db/id", ":release/gid", ":db/ident", ":Release/gid"),

    Util.map(":db/id", ":release/artists", ":db/ident", ":Release/artists"),

    Util.map(":db/id", ":release/abstractRelease", ":db/ident", ":Release/abstractRelease"),

    Util.map(":db/id", ":release/language", ":db/ident", ":Release/language"),

    Util.map(":db/id", ":release/media", ":db/ident", ":Release/media"),

    Util.map(":db/id", ":release/packaging", ":db/ident", ":Release/packaging"),

    Util.map(":db/id", ":release.packaging/jewelCase", ":db/ident", ":Release.packaging/jewelCase"),
    Util.map(":db/id", ":release.packaging/slimJewelCase", ":db/ident", ":Release.packaging/slimJewelCase"),
    Util.map(":db/id", ":release.packaging/digipak", ":db/ident", ":Release.packaging/digipak"),
    Util.map(":db/id", ":release.packaging/none", ":db/ident", ":Release.packaging/none"),
    Util.map(":db/id", ":release.packaging/keepCase", ":db/ident", ":Release.packaging/keepCase"),
    Util.map(":db/id", ":release.packaging/cardboardPaperSleeve", ":db/ident", ":Release.packaging/cardboardPaperSleeve"),
    Util.map(":db/id", ":release.packaging/other", ":db/ident", ":Release.packaging/other"),

    Util.map(":db/id", ":release/script", ":db/ident", ":Release/script"),

    Util.map(":db/id", ":release/labels", ":db/ident", ":Release/labels"),

    Util.map(":db/id", ":release/country", ":db/ident", ":Release/country"),


    // Script -----------------------------------------------------------

    Util.map(":db/id", ":script/name", ":db/ident", ":Script/name"),


    // Track ------------------------------------------------------------

    Util.map(":db/id", ":track/position", ":db/ident", ":Track/position"),

    Util.map(":db/id", ":track/duration", ":db/ident", ":Track/duration"),

    Util.map(":db/id", ":track/artistCredit", ":db/ident", ":Track/artistCredit"),

    Util.map(":db/id", ":track/name", ":db/ident", ":Track/name"),

    Util.map(":db/id", ":track/artists", ":db/ident", ":Track/artists")
  )
}