package moleculeadmin.clienttest.query.grid

import utest._

object Grid3_2 extends Base3 {


  val tests = Tests {

    test("1 2 0") {
      cols = mkCols(1, 2, 0)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(1, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, p5, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(1, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, p5, p5),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, p5, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
    }

    test("1 0 2") {
      cols = mkCols(1, 0, 2)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, __, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, __, p5),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
    }

    test("2 1 0") {
      cols = mkCols(2, 1, 0)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 1, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 1, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
    }

    test("0 1 2") {
      cols = mkCols(0, 1, 2)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 1, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, __, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 1, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, __, p5),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
    }

    test("2 0 1") {
      cols = mkCols(2, 0, 1)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 1),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 1),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 1, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
    }

    test("0 2 1") {
      cols = mkCols(0, 2, 1)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 1),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, p5, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 1),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, p5, p5),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p5, p5, p5),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, __, __),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(__, p5, p5),
        Seq(__, __, __),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p5, p5, p5),
        Seq(__, __, __),
      )
    }
  }
}
