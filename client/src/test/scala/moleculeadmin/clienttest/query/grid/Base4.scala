package moleculeadmin.clienttest.query.grid

trait Base4 extends Base {

  override def priorityResolver(
    data: Seq[Seq[Int]],
    gridType: Int
  ): (Int, Boolean) => Seq[String] = {
    val data0 = data.map(_.head)
    val data1 = data.map(_ (1))
    val data2 = data.map(_ (2))
    val data3 = data.map(_ (3))
    getPriorityResolver(gridType, Seq(
      (i: Int) => data0(i).toString,
      (i: Int) => data1(i).toString,
      (i: Int) => data2(i).toString,
      (i: Int) => data3(i).toString
    ))
  }
}
