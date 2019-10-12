package db.core.schema
import molecule.schema.definition._

@InOut(0, 10)
object TreeDefinition {

  trait Aaa {
    val attrA = oneString
    val ab    = one[Bbb]
    val ac    = one[Ccc]
    val ad    = one[Ddd]
  }

  trait Bbb {
    val attrB = oneString
    val ba    = one[Aaa]
    val bc    = one[Ccc]
    val bd    = one[Ddd]
  }

  trait Ccc {
    val attrC = oneString
    val ca    = one[Aaa]
    val cb    = one[Bbb]
    val cd    = one[Ddd]
  }

  trait Ddd {
    val attrD = oneString
    val da    = one[Aaa]
    val db    = one[Bbb]
    val dc    = one[Ccc]
  }
}
