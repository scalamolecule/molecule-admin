package moleculeadmin.clienttest.query.grid

import utest._

object Grid4 extends Base4 {


  val tests = Tests {

    test("1 2 3 4") {
      cols = mkCols(1, 2, 3, 4)

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(1, 2, 2, 2),
        Seq(1, 3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(__, __, __, p5),
        Seq(__, p3, p3, p3),
        Seq(__, p3, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(1, 2, 2, 2),
        Seq(1, 3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p3, p3, p3, p3),
        Seq(p3, p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(1, 2, 2, 2),
        Seq(2, 3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(__, __, __, p5),
        Seq(__, p3, p3, p3),
        Seq(p2, p2, p2, p2),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(1, 2, 2, 2),
        Seq(2, 3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p3, p3, p3, p3),
        Seq(p2, p2, p2, p2),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(__, __, __, p5),
        Seq(p2, p2, p2, p2),
        Seq(__, p3, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p2, p2, p2, p2),
        Seq(p3, p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p2, p2, p2, p2),
        Seq(__, p3, p3, p3),
        Seq(__, p3, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 3, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p2, p2, p2, p2),
        Seq(p3, p3, p3, p3),
        Seq(p3, p3, p3, p3),
      )
    }


    test("4 3 2 1") {
      cols = mkCols(4, 3, 2, 1)

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 1),
        Seq(2, 2, 2, 1),
        Seq(2, 2, 3, 1),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(__, p4, p3, p3),
        Seq(__, __, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 1),
        Seq(2, 2, 2, 1),
        Seq(2, 2, 3, 1),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p3, p3, p3, p3),
        Seq(p3, p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 1),
        Seq(2, 2, 2, 1),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(__, p4, p3, p3),
        Seq(p2, p2, p2, p2),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 1),
        Seq(2, 2, 2, 1),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p3, p3, p3, p3),
        Seq(p2, p2, p2, p2),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 1),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p2, p2, p2, p2),
        Seq(__, __, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 1),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p2, p2, p2, p2),
        Seq(p3, p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p2, p2, p2, p2),
        Seq(__, p4, p3, p3),
        Seq(__, __, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(2, 1, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p2, p2, p2, p2),
        Seq(p3, p3, p3, p3),
        Seq(p3, p3, p3, p3),
      )
    }


    test("3 1 2 4") {
      cols = mkCols(3, 1, 2, 4)

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(2, 1, 2, 2),
        Seq(2, 1, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(__, __, __, p5),
        Seq(p4, p4, p3, p3),
        Seq(__, __, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(2, 1, 2, 2),
        Seq(2, 1, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p3, p3, p3, p3),
        Seq(p3, p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(2, 1, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(__, __, __, p5),
        Seq(p4, p4, p3, p3),
        Seq(p2, p2, p2, p2),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(2, 1, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p3, p3, p3, p3),
        Seq(p2, p2, p2, p2),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(__, __, __, p5),
        Seq(p2, p2, p2, p2),
        Seq(__, __, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 1, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p2, p2, p2, p2),
        Seq(p3, p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p2, p2, p2, p2),
        Seq(p4, p4, p3, p3),
        Seq(__, __, p3, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 1, 2),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 3, 2),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p2, p2, p2, p2),
        Seq(p3, p3, p3, p3),
        Seq(p3, p3, p3, p3),
      )
    }

    test("3 4 1 2") {
      cols = mkCols(3, 4, 1, 2)

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 1, 1),
        Seq(2, 2, 1, 2),
        Seq(2, 2, 1, 3),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(__, p5, p5, p5),
        Seq(p4, p4, p4, p3),
        Seq(__, __, __, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 1, 1),
        Seq(2, 2, 1, 2),
        Seq(2, 2, 1, 3),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p3, p3, p3, p3),
        Seq(p3, p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 1, 1),
        Seq(2, 2, 1, 2),
        Seq(2, 2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(__, p5, p5, p5),
        Seq(p4, p4, p4, p3),
        Seq(p2, p2, p2, p2),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 1, 1),
        Seq(2, 2, 1, 2),
        Seq(2, 2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p3, p3, p3, p3),
        Seq(p2, p2, p2, p2),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 1, 1),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(__, p5, p5, p5),
        Seq(p2, p2, p2, p2),
        Seq(__, __, __, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 1, 1),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p5, p5, p5, p5),
        Seq(p2, p2, p2, p2),
        Seq(p3, p3, p3, p3),
      )

      resolve(1,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 2, 1),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p2, p2, p2, p2),
        Seq(p4, p4, p4, p3),
        Seq(__, __, __, p3),
      )
      resolve(2,
        Seq(1, 1, 1, 1),
        Seq(1, 2, 2, 1),
        Seq(2, 2, 2, 2),
        Seq(2, 2, 2, 3),
      ) ==> Seq(
        Seq(__, __, __, __),
        Seq(p2, p2, p2, p2),
        Seq(p3, p3, p3, p3),
        Seq(p3, p3, p3, p3),
      )
    }
  }
}
