package moleculeadmin.shared.x.inspiration

object Path {

  val base = if (false) "public/" else "/versionedAssets/"

  def images(img: String) = base + "images/" + img

  //  val gitIdLength = 12
  //  val searchResultBatchSize = 100
  //  val searchResultPauseSize = 500
}
