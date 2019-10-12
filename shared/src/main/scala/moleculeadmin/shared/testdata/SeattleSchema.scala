package moleculeadmin.shared.testdata

import moleculeadmin.shared.ast.schema._

trait SeattleSchema {

  implicit val nsMap: Map[String, Ns] = Map(
    "Community" -> Ns(1, "Ns", "Ns", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "name", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(2, "url", 1, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(3, "category", 2, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, Seq()),
      Attr(4, "orgtype", 1, "String", Some(Set("community", "commercial", "nonprofit", "personal")), None, None, None, None, None, None, None, Seq()),
      Attr(5, "`type`", 1, "String", Some(Set("email_list", "twitter", "facebook_page", "blog", "website", "wiki", "myspace", "ning")), None, None, None, None, None, None, None, Seq()),
      Attr(6, "neighborhood", 1, "ref", None, Some("Neighborhood"), None, None, None, None, None, None, Seq()),
    )),
    "Neighborhood" -> Ns(2, "Ref1", "Ref1", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "name", 1, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(2, "district", 1, "ref", None, Some("District"), None, None, None, None, None, None, Seq()),

    )),
    "District" -> Ns(3, "Ref2", "Ref2", None, None, Seq(
      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Seq()),
      Attr(1, "name", 1, "String", None, None, None, None, None, None, None, None, Seq()),
      Attr(2, "region", 1, "String", Some(Set("n", "ne", "e", "se", "s", "sw", "w", "nw")), None, None, None, None, None, None, None, Seq()),
    )))

}
