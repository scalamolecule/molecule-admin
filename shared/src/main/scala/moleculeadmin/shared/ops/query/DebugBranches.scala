package moleculeadmin.shared.ops.query

import moleculeadmin.shared.api.QueryApi
import moleculeadmin.shared.lib.molecule.ast.model._


trait DebugBranches extends QueryApi with Base {


  protected def debugBeforeAfter(before: Seq[Element], branch: Seq[Element], after: Seq[Element]): Unit = {
    println("=======================")
    before foreach println
    println("-----------------------")
    branch foreach println
    println("-----------------------")
    after foreach println
    println("=======================")
  }

}
