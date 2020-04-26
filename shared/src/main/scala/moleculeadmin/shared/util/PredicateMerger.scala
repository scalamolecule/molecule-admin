package moleculeadmin.shared.util

object PredicateMerger {
  implicit class And[A](val p1: A => Boolean) extends AnyVal {
    def and[B >: A](p2: B => Boolean): A => Boolean = (a: A) => p1(a) && p2(a)
    def or[B >: A](p2: B => Boolean): A => Boolean = (a: A) => p1(a) || p2(a)
  }

  implicit class And2[Pre, A](val p1: Pre => A => Boolean) extends AnyVal {
    def and[B >: A](p2: Pre => B => Boolean): Pre => A => Boolean =
      (pre: Pre) => (a: A) => p1(pre)(a) && p2(pre)(a)

    def or[B >: A](p2: Pre => B => Boolean): Pre => A => Boolean =
      (pre: Pre) => (a: A) => p1(pre)(a) || p2(pre)(a)
  }
}
