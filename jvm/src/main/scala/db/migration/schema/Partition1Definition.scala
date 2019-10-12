package db.migration.schema
import molecule.schema.definition._

@InOut(0, 5)
object Partition1Definition {

  object a {

    trait Aa {

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

  }
}
