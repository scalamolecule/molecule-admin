package moleculeadmin.clienttest.query.grid

import moleculeadmin.shared.ast.query.Col
import utest._

object Grid2_1 extends Base2 {


  val tests = Tests {

    test("1 0") {
      cols = mkCols(6, 0)

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(1, 2),
        Seq(1, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(__, __),
        Seq(__, __),
      )

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(1, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(__, __),
        Seq(p5, p5),
      )

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p5, p5),
        Seq(__, __),
      )

      resolve(1,
        Seq(1, 1),
        Seq(2, 1),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(p5, p5),
        Seq(__, __),
        Seq(__, __),
      )
    }

    test("0 1") {
      cols = mkCols(0, 6)

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 1),
        Seq(2, 1),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(__, __),
        Seq(__, __),
      )

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 1),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(__, __),
        Seq(p5, p5),
      )

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p5, p5),
        Seq(__, __),
      )

      resolve(1,
        Seq(1, 1),
        Seq(1, 2),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(p5, p5),
        Seq(__, __),
        Seq(__, __),
      )
    }
  }
}
