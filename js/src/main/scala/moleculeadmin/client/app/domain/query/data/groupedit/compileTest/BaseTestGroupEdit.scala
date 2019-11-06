package moleculeadmin.client.app.domain.query.data.groupedit.compileTest
import moleculeadmin.client.app.domain.query.QueryState.columns
import moleculeadmin.shared.ast.query.Col


class BaseTestGroupEdit(col: Col) extends TestScalaFiddle {

  val Col(_, _, _, _, attr, attrType, _, _, _, _, _, _, _, _) = col

  val optional  = attr.last == '$'

  columns() = List(
    eCol,
    col.copy(colIndex = 1, attrExpr = "orig"),
    col.copy(colIndex = 2, attrExpr = "edit")
  )

  def showResult(
    rhs: String,
    input: String,
    newValue: String,
    expected: String,
    error: String,
    scalaCode: String,
  ): Unit = {


    val attrStr    = attr + " " * (15 - attr.length)
    val errMsg     = error.replaceAllLiterally("\n", "\n       ")
    val rhsStr     = rhs.replaceAllLiterally("\n", "\n                 ")

//        println("----------------")
//        println(newValue)
//        println(error)
//        println("----------------")
//        println(expected)
//        println(errCompare)
//        println("----------------")
//        println(rhs)

    if (newValue == expected) {
      println(
        s"""$attrStr: $input
           |rhs            : $rhsStr
           |Expected result: $newValue$errMsg
           |""".stripMargin)
    } else if (error.nonEmpty && error == expected) {
      println(
        s"""$attrStr: $input
           |rhs            : $rhsStr
           |Expected error : $errMsg
           |""".stripMargin)
    } else {
      throw new IllegalArgumentException(
        s"""
           |***************** UNEXPECTED RESULT *************************
           |$attrStr: $input
           |rhs            : $rhsStr
           |Actual result  : $newValue
           |Expected       : $expected
           |Actual error   : $error
           |----- Scala code: -----
           |$scalaCode
           |""".stripMargin
      )
    }
  }
}

