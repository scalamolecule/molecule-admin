package moleculeadmin.shared.ops.query

import molecule.ast.model._


trait DebugBranches {

  protected def debugBeforeAfter(
    before: Seq[Element],
    branch: Seq[Element],
    after: Seq[Element]
  ): Unit = {
    println("=======================")
    before foreach println
    println("-----------------------")
    branch foreach println
    println("-----------------------")
    after foreach println
    println("=======================")
  }

}
