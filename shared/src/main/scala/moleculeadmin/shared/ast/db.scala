package moleculeadmin.shared.ast

object db  {
  case class Db(
    i: Int,
    name: String,
    defFilePath: String,
    open: Boolean,
    pathMsg: String,
    deleteDbMsg: String
  )
}
