package moleculeadmin.shared.testdata

import moleculeadmin.shared.ast.metaSchema._

trait SeattleSchema {

  implicit val nsMap: Map[String, MetaNs] = Map(
    "Community" -> MetaNs(1, "Ns", "Ns", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "name", 1, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(2, "url", 1, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(3, "category", 2, "String", Nil, None, Seq("fulltext"), None, None, None, None, None, Seq()),
      MetaAttr(4, "orgtype", 1, "String", Seq("community", "commercial", "nonprofit", "personal"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(5, "`type`", 1, "String", Seq("email_list", "twitter", "facebook_page", "blog", "website", "wiki", "myspace", "ning"), None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(6, "neighborhood", 1, "ref", Nil, Some("Neighborhood"), Nil, None, None, None, None, None, Seq()),
    )),
    "Neighborhood" -> MetaNs(2, "Ref1", "Ref1", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "name", 1, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(2, "district", 1, "ref", Nil, Some("District"), Nil, None, None, None, None, None, Seq()),

    )),
    "District" -> MetaNs(3, "Ref2", "Ref2", None, None, Seq(
      MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(1, "name", 1, "String", Nil, None, Nil, None, None, None, None, None, Seq()),
      MetaAttr(2, "region", 1, "String", Seq("n", "ne", "e", "se", "s", "sw", "w", "nw"), None, Nil, None, None, None, None, None, Seq()),
    )))

}
