package moleculeadmin.clienttest.query.grid

import utest._

object Grid3_3 extends Base3 {


  val tests = Tests {

    test("1 2 3") {
      cols = mkCols(1, 2, 3)
      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, p5),
        Seq(__, p4, p4),
        Seq(p3, p3, p3),
      )

      cols = mkCols(1, 2, 0)
      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, p5, p5),
        Seq(p4, p4, p4),
      )

      cols = mkCols(1, 0, 2)
      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 2),
        Seq(2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, p5),
        Seq(__, __, __),
        Seq(p3, p3, p3),
      )
    }

    test("1 2 3") {
      cols = mkCols(1, 2, 3)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(1, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, p4, p4),
        Seq(__, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(1, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, p4, p4),
        Seq(p3, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(__, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(__, p4, p4),
        Seq(__, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 2, 1),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
    }

    test("1 3 2") {
      cols = mkCols(1, 3, 2)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, __, p4),
        Seq(__, p5, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, __, p4),
        Seq(__, p5, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, __, p4),
        Seq(p3, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(__, p5, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(__, __, p4),
        Seq(__, p5, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(2, 1, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
    }

    test("2 1 3") {
      cols = mkCols(2, 1, 3)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(3, 1, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(3, 1, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p3, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 2, 1),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 2, 1),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 1),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 2, 1),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
    }

    test("3 1 2") {
      cols = mkCols(3, 1, 2)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 1, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, __, p4),
        Seq(p5, p5, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 1, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, __, p4),
        Seq(p3, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p5, p5, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(__, __, p4),
        Seq(p5, p5, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(1, 2, 2),
        Seq(2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
    }

    test("2 3 1") {
      cols = mkCols(2, 3, 1)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(3, 2, 1),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(3, 2, 1),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p3, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 1),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 2),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(2, 1, 2),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 1, 2),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(2, 1, 2),
        Seq(3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
    }

    test("3 2 1") {
      cols = mkCols(3, 2, 1)

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 3, 1),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, p4, p4),
        Seq(p5, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 3, 1),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(__, p4, p4),
        Seq(p3, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 1),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p4, p4, p4),
        Seq(p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 2),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p5, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 1),
        Seq(1, 2, 2),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
      )

      resolve(1,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 2),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(__, p4, p4),
        Seq(p5, p4, p4),
      )
      resolve(2,
        Seq(1, 1, 1),
        Seq(1, 1, 2),
        Seq(1, 2, 2),
        Seq(2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __),
        Seq(p3, p3, p3),
        Seq(p4, p4, p4),
        Seq(p4, p4, p4),
      )
    }
  }
}
