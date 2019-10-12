package moleculeadmin.shared.util

object PredicateMerger {
  implicit class And[A](val p1: A => Boolean) extends AnyVal {
    def and[B >: A](p2: B => Boolean): A => Boolean = (a: A) => p1(a) && p2(a)
    def or[B >: A](p2: B => Boolean): A => Boolean = (a: A) => p1(a) || p2(a)
  }
}
