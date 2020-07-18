package moleculeadmin.clienttest.query.grid

trait Base5 extends Base {

  override def priorityResolver(
    data: Seq[Seq[Int]],
    gridType: Int
  ): (Int, Boolean) => Seq[String] = {
    val data0 = data.map(_.head)
    val data1 = data.map(_ (1))
    val data2 = data.map(_ (2))
    val data3 = data.map(_ (3))
    val data4 = data.map(_ (4))
    getPriorityResolver(gridType, Seq(
      (i: Int) => data0(i).toString,
      (i: Int) => data1(i).toString,
      (i: Int) => data2(i).toString,
      (i: Int) => data3(i).toString,
      (i: Int) => data4(i).toString
    ))
  }
}
