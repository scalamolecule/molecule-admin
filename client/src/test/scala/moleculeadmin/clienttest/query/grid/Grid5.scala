package moleculeadmin.clienttest.query.grid

import utest._

object Grid5 extends Base5 {


  val tests = Tests {

    // Random order...
    test("4 2 5 1 3") {
      cols = mkCols(4, 2, 5, 1, 3)

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 1, 2),
        Seq(2, 1, 3, 1, 2),
        Seq(2, 2, 4, 1, 2),
        Seq(2, 2, 5, 1, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(__, __, p5, p5, p3),
        Seq(p4, p4, p4, p4, p4),
        Seq(__, p2, p2, p2, p2),
        Seq(__, __, p5, p5, p5),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 1, 2),
        Seq(2, 1, 3, 1, 2),
        Seq(2, 2, 4, 1, 2),
        Seq(2, 2, 5, 1, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p3, p3, p3, p3, p3),
        Seq(p4, p4, p4, p4, p4),
        Seq(p2, p2, p2, p2, p2),
        Seq(p5, p5, p5, p5, p5),
      )

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 1, 2),
        Seq(2, 1, 3, 1, 2),
        Seq(2, 2, 4, 1, 2),
        Seq(2, 2, 5, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(__, __, p5, p5, p3),
        Seq(p4, p4, p4, p4, p4),
        Seq(__, p2, p2, p2, p2),
        Seq(p1, p1, p1, p1, p1),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 1, 2),
        Seq(2, 1, 3, 1, 2),
        Seq(2, 2, 4, 1, 2),
        Seq(2, 2, 5, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p3, p3, p3, p3, p3),
        Seq(p4, p4, p4, p4, p4),
        Seq(p2, p2, p2, p2, p2),
        Seq(p1, p1, p1, p1, p1),
      )

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 1, 2),
        Seq(2, 1, 3, 1, 2),
        Seq(2, 2, 4, 2, 2),
        Seq(2, 2, 5, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(__, __, p5, p5, p3),
        Seq(p4, p4, p4, p4, p4),
        Seq(p1, p1, p1, p1, p1),
        Seq(__, __, p5, p5, p5),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 1, 2),
        Seq(2, 1, 3, 1, 2),
        Seq(2, 2, 4, 2, 2),
        Seq(2, 2, 5, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p3, p3, p3, p3, p3),
        Seq(p4, p4, p4, p4, p4),
        Seq(p1, p1, p1, p1, p1),
        Seq(p5, p5, p5, p5, p5),
      )

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 1, 2),
        Seq(2, 1, 3, 2, 2),
        Seq(2, 2, 4, 2, 2),
        Seq(2, 2, 5, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(__, __, p5, p5, p3),
        Seq(p1, p1, p1, p1, p1),
        Seq(__, p2, p2, p2, p2),
        Seq(__, __, p5, p5, p5),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 1, 2),
        Seq(2, 1, 3, 2, 2),
        Seq(2, 2, 4, 2, 2),
        Seq(2, 2, 5, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p3, p3, p3, p3, p3),
        Seq(p1, p1, p1, p1, p1),
        Seq(p2, p2, p2, p2, p2),
        Seq(p5, p5, p5, p5, p5),
      )

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 2, 2),
        Seq(2, 1, 3, 2, 2),
        Seq(2, 2, 4, 2, 2),
        Seq(2, 2, 5, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p1, p1, p1, p1, p1),
        Seq(p4, p4, p4, p4, p4),
        Seq(__, p2, p2, p2, p2),
        Seq(__, __, p5, p5, p5),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 1, 2, 2, 2),
        Seq(2, 1, 3, 2, 2),
        Seq(2, 2, 4, 2, 2),
        Seq(2, 2, 5, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p1, p1, p1, p1, p1),
        Seq(p4, p4, p4, p4, p4),
        Seq(p2, p2, p2, p2, p2),
        Seq(p5, p5, p5, p5, p5),
      )
    }


    test("2 5 1 4 3") {
      cols = mkCols(2, 5, 1, 4, 3)

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 1, 1, 2),
        Seq(1, 3, 1, 2, 2),
        Seq(2, 4, 1, 2, 2),
        Seq(2, 5, 1, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(__, p5, p5, p5, p3),
        Seq(__, p5, p5, p4, p4),
        Seq(p2, p2, p2, p2, p2),
        Seq(__, p5, p5, p5, p5),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 1, 1, 2),
        Seq(1, 3, 1, 2, 2),
        Seq(2, 4, 1, 2, 2),
        Seq(2, 5, 1, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p3, p3, p3, p3, p3),
        Seq(p4, p4, p4, p4, p4),
        Seq(p2, p2, p2, p2, p2),
        Seq(p5, p5, p5, p5, p5),
      )

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 1, 1, 2),
        Seq(1, 3, 1, 2, 2),
        Seq(2, 4, 1, 2, 2),
        Seq(2, 5, 2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(__, p5, p5, p5, p3),
        Seq(__, p5, p5, p4, p4),
        Seq(p2, p2, p2, p2, p2),
        Seq(p1, p1, p1, p1, p1),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 1, 1, 2),
        Seq(1, 3, 1, 2, 2),
        Seq(2, 4, 1, 2, 2),
        Seq(2, 5, 2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p3, p3, p3, p3, p3),
        Seq(p4, p4, p4, p4, p4),
        Seq(p2, p2, p2, p2, p2),
        Seq(p1, p1, p1, p1, p1),
      )

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 1, 1, 2),
        Seq(1, 3, 1, 2, 2),
        Seq(2, 4, 2, 2, 2),
        Seq(2, 5, 2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(__, p5, p5, p5, p3),
        Seq(__, p5, p5, p4, p4),
        Seq(p1, p1, p1, p1, p1),
        Seq(__, p5, p5, p5, p5),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 1, 1, 2),
        Seq(1, 3, 1, 2, 2),
        Seq(2, 4, 2, 2, 2),
        Seq(2, 5, 2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p3, p3, p3, p3, p3),
        Seq(p4, p4, p4, p4, p4),
        Seq(p1, p1, p1, p1, p1),
        Seq(p5, p5, p5, p5, p5),
      )

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 1, 1, 2),
        Seq(1, 3, 2, 2, 2),
        Seq(2, 4, 2, 2, 2),
        Seq(2, 5, 2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(__, p5, p5, p5, p3),
        Seq(p1, p1, p1, p1, p1),
        Seq(p2, p2, p2, p2, p2),
        Seq(__, p5, p5, p5, p5),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 1, 1, 2),
        Seq(1, 3, 2, 2, 2),
        Seq(2, 4, 2, 2, 2),
        Seq(2, 5, 2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p3, p3, p3, p3, p3),
        Seq(p1, p1, p1, p1, p1),
        Seq(p2, p2, p2, p2, p2),
        Seq(p5, p5, p5, p5, p5),
      )

      resolve(1,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 2, 1, 2),
        Seq(1, 3, 2, 2, 2),
        Seq(2, 4, 2, 2, 2),
        Seq(2, 5, 2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p1, p1, p1, p1, p1),
        Seq(__, p5, p5, p4, p4),
        Seq(p2, p2, p2, p2, p2),
        Seq(__, p5, p5, p5, p5),
      )
      resolve(2,
        Seq(1, 1, 1, 1, 1),
        Seq(1, 2, 2, 1, 2),
        Seq(1, 3, 2, 2, 2),
        Seq(2, 4, 2, 2, 2),
        Seq(2, 5, 2, 2, 2),
      ) ==> Seq(
        Seq(__, __, __, __, __),
        Seq(p1, p1, p1, p1, p1),
        Seq(p4, p4, p4, p4, p4),
        Seq(p2, p2, p2, p2, p2),
        Seq(p5, p5, p5, p5, p5),
      )
    }
  }
}
