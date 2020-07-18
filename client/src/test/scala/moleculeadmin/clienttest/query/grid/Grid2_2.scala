package moleculeadmin.clienttest.query.grid

import moleculeadmin.shared.ast.query.Col
import utest._

object Grid2_2 extends Base2 {


  val tests = Tests {

    test("1 2") {
      cols = mkCols(1, 2)

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(1, 2),
        Seq(1, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(__, p5),
        Seq(__, __),
      )
      resolve(2,
        Seq(1, 1),
        Seq(1, 1),
        Seq(1, 2),
        Seq(1, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p5, p5),
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
        Seq(__, p5),
        Seq(p4, p4),
      )
      resolve(2,
        Seq(1, 1),
        Seq(1, 1),
        Seq(1, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p5, p5),
        Seq(p4, p4),
      )

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p4, p4),
        Seq(__, __),
      )
      resolve(2,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p4, p4),
        Seq(__, __),
      )

      resolve(1,
        Seq(1, 1),
        Seq(2, 1),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(p4, p4),
        Seq(__, p5),
        Seq(__, __),
      )
      resolve(2,
        Seq(1, 1),
        Seq(2, 1),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(p4, p4),
        Seq(p5, p5),
        Seq(__, __),
      )
    }

    test("2 1") {
      cols = mkCols(2, 1)

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 1),
        Seq(2, 1),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p5, p5),
        Seq(__, __),
      )
      resolve(2,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 1),
        Seq(2, 1),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p5, p5),
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
        Seq(p5, p5),
        Seq(p4, p4),
      )
      resolve(2,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 1),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p5, p5),
        Seq(p4, p4),
      )

      resolve(1,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p4, p4),
        Seq(__, __),
      )
      resolve(2,
        Seq(1, 1),
        Seq(1, 1),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(__, __),
        Seq(p4, p4),
        Seq(__, __),
      )

      resolve(1,
        Seq(1, 1),
        Seq(1, 2),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(p4, p4),
        Seq(p5, p5),
        Seq(__, __),
      )
      resolve(2,
        Seq(1, 1),
        Seq(1, 2),
        Seq(2, 2),
        Seq(2, 2),
      ) ==> Seq(
        Seq(__, __),
        Seq(p4, p4),
        Seq(p5, p5),
        Seq(__, __),
      )
    }
  }
}
