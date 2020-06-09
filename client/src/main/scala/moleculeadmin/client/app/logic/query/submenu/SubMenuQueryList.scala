package moleculeadmin.client.app.logic.query.submenu

import moleculeadmin.client.app.logic.query.Callbacks
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.html.query.SubMenuElements
import moleculeadmin.shared.ast.query.QueryDTO
import moleculeadmin.shared.ops.query.MoleculeOps
import org.scalajs.dom.html.LI
import rx.Ctx


case class SubMenuQueryList()(implicit val ctx: Ctx.Owner)
  extends Callbacks with SubMenuElements with MoleculeOps {


  def render: LI = {

    // Organize saved molecules by part/ns/molecule hierarchy
    val queriesByPartNs = savedQueries.sortBy(_.molecule).foldLeft(
      "": String, "": String,
      Seq.empty[(String, Seq[(String, Seq[(String, QueryDTO)])])]
    ) {
      case ((p0, _, pp), q@QueryDTO(m, p, ns, _, _, _, _)) if p0 != p =>
        (p, ns, pp :+ p -> Seq(ns -> Seq(m -> q)))

      case ((_, ns0, pp), q@QueryDTO(m, p, ns, _, _, _, _)) if ns0 != ns =>
        val (curPart, curNss) = pp.last
        val newNs             = ns -> Seq(m -> q)
        (p, ns, pp.init :+ (curPart -> (curNss :+ newNs)))

      case ((_, _, pp), q@QueryDTO(m, p, ns, _, _, _, _)) =>
        val (curPart, curNss) = pp.last
        val (curNs, curMols)  = curNss.last
        val updatedNs         = curNs -> (curMols :+ (m -> q))
        val updatedPart       = curPart -> (curNss.init :+ updatedNs)
        (p, ns, pp.init :+ updatedPart)
    }._3

    val favoriteQueries = getFavoriteQueries

    _subMenuQueryList(
      curMolecule.now,
      newFav,
      queriesByPartNs,

      recentQueries.sortBy(_.molecule),
      savedQueries,
      favoriteQueries,

      savedQueries.map(_.molecule),
      favoriteQueries.map(_.molecule),

      useQueryCallback,
      upsertQueryCallback,
      favoriteQueryCallback,
      unfavoriteQueryCallback,
      retractQueryCallback
    ).render
  }
}
