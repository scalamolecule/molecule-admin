package db.migration.schema

import molecule.schema.definition._

@InOut(0, 5)
object Partition2Definition {

  object a {

    trait Aa {
      val aa1 = oneInt
      val abb = one[b.Bb]
      val abc = one[b.Bc]
      val abd = one[b.Bd]
    }
  }

  object b {

    trait Bb {
      val bb1 = oneInt
      val bb2 = oneInt
      val bb3 = oneInt
      val bb4 = oneInt
    }

    trait Bc {
      val bc1 = oneInt
    }

    trait Bd {
      val bd1 = oneInt
    }
  }

  object c {
    trait Cc {
      val cc1 = oneInt
    }
  }
}
