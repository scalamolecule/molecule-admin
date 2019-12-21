package moleculeadmin.client.app.domain.query.submenu
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.SubMenuElements
import moleculeadmin.shared.ast.query.QueryData
import moleculeadmin.shared.ops.query.MoleculeOps
import org.scalajs.dom.html.LI
import rx.{Ctx, Rx}
import scalatags.JsDom


case class QueryList(db: String)(implicit val ctx: Ctx.Owner)
  extends Callbacks with SubMenuElements with MoleculeOps {

  def dynRender: Rx.Dynamic[JsDom.TypedTag[LI]] = Rx {

    // Organize saved molecules by part/ns/molecule hierarchy
    val queriesByPartNs = savedQueries.now.sortBy(_.molecule).foldLeft(
      "", "",
      Seq.empty[(String, Seq[(String, Seq[(String, QueryData)])])]
    ) {
      case ((p0, _, pp), q@QueryData(m, p, ns, _, _, _, _)) if p0 != p =>
        (p, ns, pp :+ p -> Seq(ns -> Seq(m -> q)))

      case ((_, ns0, pp), q@QueryData(m, p, ns, _, _, _, _)) if ns0 != ns =>
        val (curPart, curNss) = pp.last
        val newNs             = ns -> Seq(m -> q)
        (p, ns, pp.init :+ (curPart -> (curNss :+ newNs)))

      case ((_, _, pp), q@QueryData(m, p, ns, _, _, _, _)) =>
        val (curPart, curNss) = pp.last
        val (curNs, curMols)  = curNss.last
        val updatedNs         = curNs -> (curMols :+ (m -> q))
        val updatedPart       = curPart -> (curNss.init :+ updatedNs)
        (p, ns, pp.init :+ updatedPart)
    }._3


    val favoriteQueries = savedQueries.now.filter(_.isFavorite).sortBy(_.molecule)

    val newFav: Seq[QueryData] =
      if (curMolecule.now.isEmpty
//        || savedQueries.now.exists(_.molecule == curMolecule.now)
      ) {
        Nil
      } else {
        val (part, ns) = getPartNs(curMolecule.now)
        Seq(
          QueryData(
            curMolecule.now,
            part, ns, true,
            showGrouped.now,
            groupedCols.now,
            colSettings(columns.now)
          )
        )
      }

    //    println("QueryList " + curMolecule.now)
    //    println("newFav " + newFav)

    _subMenuQueryList(
      curMolecule.now,
      newFav,
      queriesByPartNs,

      recentQueries.now.sortBy(_.molecule),
      savedQueries.now,
      favoriteQueries,

      savedQueries.now.map(_.molecule),
      favoriteQueries.map(_.molecule),

      useQueryCallback,
      upsertQueryCallback,
      favoriteQueryCallback,
      unfavoriteQueryCallback,
      retractQueryCallback,
    )
  }
}
