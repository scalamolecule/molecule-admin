package db.migration.schema
import molecule.schema.definition._

@InOut(0, 5)
object PartitionDefinition {

  object a {

    trait Aa {

    }
  }

  object b {

    trait Bb {
      val bb1 = oneInt
      val bb2 = oneInt
    }

    trait Bc {
      val bc1 = oneInt
    }
  }

  object c {

  }
}
